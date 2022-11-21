package evo.derivation.internal.arrays

extension [A](v: IArray[A])
    inline def forEachInline(inline f: (A) => Boolean): Unit =
        val n        = v.length
        var i        = 0
        var continue = true
        while continue && i < n do
            f(v(i))
            i += 1
    end forEachInline

    inline def forEachInline2[B](vb: IArray[B])(inline f: (A, B) => Boolean): Unit =
        val n        = v.length
        var i        = 0
        var continue = true
        while continue && i < n do
            continue = f(v(i), vb(i))
            i += 1
    end forEachInline2
end extension
