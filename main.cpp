#include <iostream>
#include <tuple>
#include <unordered_map>

using namespace std;

struct Obj1
{};
struct Obj2
{};

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

struct Group : VisitableGroup<VisitableGroup<Obj1, Obj2>, Obj1, Obj2>
{
  using VisitableGroup<VisitableGroup<Obj1, Obj2>, Obj1, Obj2>::VisitableChildren;
  using VisitableGroup<VisitableGroup<Obj1, Obj2>, Obj1, Obj2>::getChildMap;
  using VisitableGroup<VisitableGroup<Obj1, Obj2>, Obj1, Obj2>::MapType;
};

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

static_assert (HasGetChildren<const Group&>::value, "");

template<typename VisitFunc>
struct FunctorTypeTraits;

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

template<typename VisitableFunc>
struct VisitFunc<VisitableFunc, true> : VisitableFunc
{
  VisitFunc(VisitableFunc func) : VisitableFunc(std::move(func)) {}

  template<size_t... Size, typename Type>
  void visitChildren(std::index_sequence<Size...>, Type &&param) const {
    auto visitor = [](auto &map) { std::cout << typeid (decltype (map)).name() << std::endl; };
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
  visit(group, [](const Group&) {}, [&](const Obj1&) {}, [&](const Obj2&) {});
  return 0;
}
