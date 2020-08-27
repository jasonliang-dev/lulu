{-# LANGUAGE QuasiQuotes #-}

module LuluInclude
  ( includeHeader
  )
where

import           Text.RawString.QQ

includeHeader :: String
includeHeader = [r|
const add = (a, b) => a + b
const sub = (a, b) => a - b
const mul = (a, b) => a * b
const div = (a, b) => a / b
const mod = (a, b) => a % b
const inc = (a) => a + 1
const dec = (a) => a - 1

const eq = (a, b) => a == b
const lt = (a, b) => a < b
const lte = (a, b) => a <= b
const gt = (a, b) => a > b
const gte = (a, b) => a >= b
const and = (pred1, pred2) => pred1 && pred2
const or = (pred1, pred2) => pred1 || pred2
const xor = (pred1, pred2) => pred1 ^ pred2

const list = (...elems) => elems
const nth = (i, xs) => xs[i]
const range = (min, max) => Array.from(Array(max - min)).map((_, i) => i)
const length = (xs) => xs.length
const concat = (xs, ...rest) => xs.concat(...rest)
const join = (sep, xs) => xs.join(sep)

const fromList = (arr) => arr.reduce((o, [k, v]) => ({ ...o, [k]: v }), {})
const dict = (...arr) => fromList(arr)
const assoc = (k, v, m) => ({ ...m, [k]: v })
const dissoc = (k, v, m) => ({ ...m, [k]: undefined })
const prop = nth

const map = (fn, xs) => xs.map(fn)
const filter = (fn, xs) => xs.filter(fn)
const reduce = (fn, zero, xs) => xs.reduce(fn, zero)

const always = (a) => () => a
|]
