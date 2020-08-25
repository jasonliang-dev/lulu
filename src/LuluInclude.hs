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
const isEmpty = (a) => !!a

const ifElse = (pred, a, b) => pred ? a : b
const ifElse_ = (pred, a = () => {}, b = () => {}) => pred ? a() : b()
const when = (pred, a) => pred ? a : undefined
const when_ = (pred, a = () => {}) => pred ? a() : undefined

const list = (...elems) => elems
const range = (min, max) => Array.from(Array(max - min)).map((_, i) => i)
const length = (xs) => xs.length
const concat = (xs, ys) => xs.concat(ys)
const join = (sep, xs) => xs.join(sep)

const assoc = (k, v, m) => ({ ...m, [k]: v })
const dissoc = (k, v, m) => ({ ...m, [k]: undefined })
const path = (k, m) => m[k]

const map = (fn, xs) => xs.map(fn)
const filter = (fn, xs) => xs.filter(fn)
const reduce = (fn, zero, xs) => xs.reduce(fn, zero)
|]
