type Maybe<T> = Nothing<T> | Just<T>
type Either<X, Y> = Left<X> | Right<Y>

class EitherMonad<X, Y> implements Monad<Either<X, Y>, Y>{
    pure = (x) => new Right(x)
    $ = (a, f) => (a instanceof Right)
        ? f(a.value)
        : a
}

class Right<Y> extends EitherMonad<any, Y>{
    constructor(public value: Y) { super() };
};

class Left<X> extends EitherMonad<X, any>{
    constructor(public value: X) { super() };
};

class MaybeMonad<V> implements Monad<Maybe<V>, V> {
    pure = (x) => new Just(x)
    $ = (a, f) => (a instanceof Just)
        ? f(a.value)
        : a
}

type monadF = (x) => any

interface Monad<M, V> {
    pure: (x: V) => M
    $: (a: V, f: monadF) => M
}

class Nothing<T> extends MaybeMonad<T> {
 };

class Just<T> extends MaybeMonad<T> {
    constructor(public value: T) { super() };
}


const compose = <R>(...fns: Array<(a: R) => R>) =>
  fns.reverse().splice(1).reduce((prevFn, nextFn) => value => prevFn(nextFn(value)), fns.pop());

const doMonad = <V, M>(initialValue: V, monad: Monad<M, V>) => (...fns: monadF[]): Monad<M, V> => {
    const sequence = fns.map(fn => (x) => monad.$(x, fn))
    return compose(...sequence)(monad.pure(initialValue))
}

const shouldCheckForEnemy = (x: boolean) => x ? new Just(x) : new Nothing()
const isEnemyInSight = (x: boolean) => x ? new Just(x) : new Nothing()
const rocketsAreLoaded = (x: boolean) => x ? new Just(x) : new Nothing()
const aimForTarget = () => new Right(true)
const shotAtTarget = () => new Left("damn, missed")

const rocketSystemResult = doMonad(true, new EitherMonad)(
    x => new Right(doMonad(x, new MaybeMonad)(
        k => shouldCheckForEnemy(k),
        k => isEnemyInSight(k),
        k => rocketsAreLoaded(k)
    )),
    x => (x instanceof Just && x.value === true)
        ? aimForTarget()
        : new Left("rocket system not used"),
    x => x
        ? shotAtTarget()
        : new Left("could not aim")
)

const simpleMathsResult = doMonad(7, new MaybeMonad)(
    x => new Just(x + 2),
    x => new Just(x / 2),
)

console.log(JSON.stringify(rocketSystemResult))
console.log(JSON.stringify(simpleMathsResult))