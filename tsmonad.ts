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

const checkForEnemy = (shouldInitializeCheck: boolean) => shouldInitializeCheck ? new Just("enemy1") : new Nothing()
const chooseAmmoForTarget = (x: string) => x === "enemy1" ? new Just(["rockets", x]) : new Nothing()
const getTargetHandlerIfAmmoLoaded = (x: string[]) => x[0] === "rockets" ? new Just(x[1]) : new Nothing()
const aimForTarget = (x: string) => new Right(x)
const shotAtTarget = (x: string) => new Left(x + " missed")

const rocketSystemResult = doMonad(true, new EitherMonad)(
    init => new Right(doMonad(init, new MaybeMonad)(
        init => checkForEnemy(init),
        enemy => chooseAmmoForTarget(enemy),
        ammo => getTargetHandlerIfAmmoLoaded(ammo)
    )),
    target => (target instanceof Just)
        ? aimForTarget(target.value)
        : new Left("rocket system not used"),
    target => shotAtTarget(target)
)

// const rocketSystemResultImperative = (init: boolean) => {
//     const enemy = checkForEnemy(init);
//     let target = null;
//     if(enemy !== null){
//         const ammo = chooseAmmoForTarget(enemy);
//         if(ammo !== null){
//             target = getTargetHandlerIfAmmoLoaded(ammo);
//         }
//     }
//     if(target === null){
//         return "rocket system not used"
//     }else{
//         const aim = aimForTarget(target)
//         if(aim !== null){
//             return shotAtTarget(target)
//         }
//     }
// }

const simpleMathsResult = doMonad(7, new MaybeMonad)(
    x => new Just(x + 2),
    x => new Just(x / 2),
)

console.log(JSON.stringify(rocketSystemResult))
console.log(JSON.stringify(simpleMathsResult))