canHit []       = False
canHit (q : qs) = forEachQueen q qs || canHit qs
  where
    forEachQueen _ []       = False
    forEachQueen q (w : ws) = hit q w || forEachQueen q ws
      where
        hit (qh, qv) (wh, wv) =
            qh == wh || qv == wv || abs (qh - wh) == abs (qv - wv)
