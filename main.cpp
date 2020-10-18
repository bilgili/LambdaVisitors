#include <iostream>
#include <tuple>
#include <unordered_map>

using namespace std;

/** Possible child objects */
struct ChildObj1
{};
struct ChildObj2
{};

/* A base group object that can host child objects, possible children known in compile time */
template<typename... Visitables>
struct VisitableGroup
{
  using VisitableChildren = std::tuple<Visitables...>;
  template<typename T>
  using Map = std::unordered_map<size_t, T>;
  using MapType = std::tuple<Map<Visitables>...>;
  const MapType &getChildMap() const noexcept { return _chidMap;  }
  MapType &getChildMap() noexcept { return _chidMap;  }
  MapType _chidMap;
};

/* A group object that can host child objects, possible children known in compile time */
struct Group : VisitableGroup<VisitableGroup<ChildObj1, ChildObj2>, ChildObj1, ChildObj2>
{
  using VisitableGroup<VisitableGroup<ChildObj1, ChildObj2>, ChildObj1, ChildObj2>::VisitableChildren;
  using VisitableGroup<VisitableGroup<ChildObj1, ChildObj2>, ChildObj1, ChildObj2>::getChildMap;
  using VisitableGroup<VisitableGroup<ChildObj1, ChildObj2>, ChildObj1, ChildObj2>::MapType;
};

/* SFINAE class for requesting a group class to have a const getChildMap function */
template<typename T>
struct HasGetChildren
{
private:
  typedef std::true_type yes;
  typedef std::false_type no;

  template<typename U>
  static yes test(int) {
    static_cast<const typename U::MapType&(U::*)() const>(&U::getChildMap);
    return yes();
  }

  template<typename> static no test(...);

public:
  static constexpr bool value = std::is_same<decltype(test<T>(0)),yes>::value;
};

template<typename VisitFunc>
struct FunctorTypeTraits;

/* Based on the functor it can be decided to execute lambdas differently */
template<typename Ret, typename T, typename... Params>
struct FunctorTypeTraits<Ret (T::*)(Params...)>
{
  using Type = std::tuple_element_t<0, std::tuple<Params...>>;
  constexpr static bool hasChildren = HasGetChildren<std::remove_reference_t<Type>>::value;
};

template<typename Ret, typename T, typename... Params>
struct FunctorTypeTraits<Ret (T::*)(Params...) const>
{
  using Type = std::tuple_element_t<0, std::tuple<Params...>>;
  constexpr static bool hasChildren = HasGetChildren<std::remove_reference_t<Type>>::value;
};

template<typename Ret, typename... Params>
struct FunctorTypeTraits<Ret (*)(Params...)>
{
  using Type = std::tuple_element_t<0, std::tuple<Params...>>;
  constexpr static bool hasChildren = HasGetChildren<std::remove_reference_t<Type>>::value;
};

template<typename VisitableFunc,
         bool hasChildren = FunctorTypeTraits<decltype(&VisitableFunc::operator())>::hasChildren>
struct VisitFunc;

template<typename... VisitableFuncs>
struct Visitors;

/* A lambda inherited visitors class */
template<typename VisitableFunc, typename... VisitableFuncs>
struct Visitors<VisitableFunc, VisitableFuncs...> : VisitFunc<VisitableFunc>,
                                                    Visitors<VisitableFuncs...>
{
  using VisitFunc<VisitableFunc>::operator();
  Visitors(VisitableFunc func, VisitableFuncs... funcs)
      : VisitFunc<VisitableFunc>(std::move(func)), Visitors<VisitableFuncs...>(std::move(funcs)...) {
  }
};

template<typename VisitableFunc>
struct Visitors<VisitableFunc> : VisitFunc<VisitableFunc>
{
  using VisitFunc<VisitableFunc>::operator();
  Visitors(VisitableFunc func) : VisitFunc<VisitableFunc>(std::move(func)) {}
};

template<typename... Funcs>
auto makeVisitors(Funcs &&... funcs) {
  return Visitors<Funcs...>(std::forward<Funcs>(funcs)...);
}

template<typename Visitable, typename... Funcs>
void visit(Visitable &visitable, Funcs &&... funcs) {
  auto visitors = makeVisitors(std::forward<Funcs>(funcs)...);
  visitors(visitable);
}

/* A custom lambda based on original lambda, with extra children traversal possibility */
template<typename VisitableFunc>
struct VisitFunc<VisitableFunc, true> : VisitableFunc
{
  VisitFunc(VisitableFunc func) : VisitableFunc(std::move(func)) {}

  template<size_t... Size, typename Type>
  void visitChildren(std::index_sequence<Size...>, Type &&param) const {
    const auto visitor = [](auto &map)
    {
      // Visit your children
      std::cout << typeid (decltype (map)).name() << std::endl;
    };
    const int32_t visits[] = {(visitor(std::get<Size>(param.getChildMap())), 0)...};
    (void) visits;
  }

  template<typename Type, typename... Params>
  auto operator()(Type &&param, Params &&... params) const {
    using ChildrenTuple = typename std::decay_t<Type>::VisitableChildren;
    constexpr size_t size = std::tuple_size<ChildrenTuple>::value;
    std::cout << "has children" << std::endl;
    visitChildren(std::make_index_sequence<size>{}, std::forward<Type>(param));
    return VisitableFunc::operator()(std::forward<Type>(param), std::forward<Type>(params)...);
  }
};

/* A custom lambda based on original lambda, for nodes whic dont have traversal */
template<typename VisitableFunc>
struct VisitFunc<VisitableFunc, false> : VisitableFunc
{
  VisitFunc(VisitableFunc func) : VisitableFunc(std::move(func)) {}

  template<typename... Params>
  auto operator()(Params &&... params) const {
    std::cout << "has no children" << std::endl;
    return VisitableFunc::operator()(std::forward<Params>(params)...);
  }
};

int main()
{
  Group group;
  /*
   * Visit group recursively. All possible lambdas here but better (and right) approach would be the ones
   * provided by the group class.
   */
  visit(group, [](const Group&) {}, [&](const ChildObj1&) {}, [&](const ChildObj2&) {});
  return 0;
}
