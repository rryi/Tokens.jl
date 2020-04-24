"""
brute force search without larger skips
"""
function _searchindex_naive(s::ByteArray, t::ByteArray, i::Integer,sv::Vector)
    n = sizeof(t)
    m = sizeof(s)

    if n == 0
        return 1 <= i <= m+1 ? max(1, i) : 0
    elseif m == 0
        return 0
    elseif n == 1
        return something(findnext(isequal(_nthbyte(t,1)), s, i), 0)
    end

    w = m - n
    if w < 0 || i - 1 > w
        return 0
    end

    tlast = _nthbyte(t,n)
    if DOSTATS loops = 0 end
    if DOSTATS bloomtests = 0 end
    if DOSTATS bloomskips = 0 end
    i -= 1
    while i <= w
        if DOSTATS loops += 1 end
        if _nthbyte(s,i+n) == tlast
            # check candidate
            j = 1
            while j < n
                if _nthbyte(s,i+j) != _nthbyte(t,j)
                    break
                end
                j += 1
                # match found?
                if j == n
                    if DOSTATS sv[Int(SFloops)] = loops; sv[Int(SFbloomtests)] = bloomtests; sv[Int(SFbloomskips)] = bloomskips; sv[Int(SFbloombits)] = 0 end
                    return i+1
                end
            end
        end
        i += 1
    end
    if DOSTATS sv[Int(SFloops)] = loops end
    if DOSTATS sv[Int(SFbloomtests)] = bloomtests end
    if DOSTATS sv[Int(SFbloomskips)] = bloomskips end
    if DOSTATS sv[Int(SFbloombits)] = 0 end
    0
end
