module Basic where 
    import qualified Numeric as N

    readHexs :: (Eq a, Num a) =>String-> Maybe (a, String)
    readHexs s =
        case N.readHex s of
            [] -> Nothing
            ((a, q):_) -> Just (a, q)

    readHex ::(Eq a, Num a) =>String-> Maybe a
    readHex =(<$>) fst . readHexs

    readFloats ::(RealFrac a) =>String-> Maybe (a, String)
    readFloats s =
        case N.readSigned N.readFloat s of
                [] -> Nothing
                ((a, q):_) -> Just (a, q)

    readFloat ::(RealFrac a) =>String-> Maybe a
    readFloat = (<$>) fst . readFloats