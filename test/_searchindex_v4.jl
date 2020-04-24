"""
variant: test bloomfilter first
"""
function _searchindex_v4(s::ByteArray, t::ByteArray, i::Integer,sv::Vector)
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

    skip = n
    tlast = _nthbyte(t,n)
    bloom_mask = UInt64(_search_bloom_mask(tlast))
    for j in 1:n-1
        bloom_mask |= _search_bloom_mask(_nthbyte(t,j))
        if _nthbyte(t,j) == tlast
            skip = n - j
        end
    end
    if DOSTATS loops = 0 end
    if DOSTATS bloomtests = 0 end
    if DOSTATS bloomskips = 0 end
    i +=n-1
    while i < m
        if DOSTATS loops += 1 end
        if DOSTATS bloomtests += 1 end
        if bloom_mask & _search_bloom_mask(_nthbyte(s,i)) == 0
            if DOSTATS bloomskips += 1 end
            i += n
        else
            if _nthbyte(s,i) == tlast
                # check candidate
                j = 1
                while j < n
                    if _nthbyte(s,i-n+j) != _nthbyte(t,j)
                        break
                    end
                    j += 1
                    # match found?
                    if j == n
                        if DOSTATS sv[Int(SFloops)] = loops; sv[Int(SFbloomtests)] = bloomtests; sv[Int(SFbloomskips)] = bloomskips; sv[Int(SFbloombits)] = bitcount(bloom_mask) end
                        return i-n+1
                    end
                end
                # no match: skip and test bloom
                i += skip
            else
                i +=1
            end
        end
    end
    if i==m
        # test end match
        j = 1
        while j <= n
            if _nthbyte(s,i-n+j) != _nthbyte(t,j)
                break # not found
            end
            if j == n
                if DOSTATS sv[Int(SFloops)] = loops; sv[Int(SFbloomtests)] = bloomtests; sv[Int(SFbloomskips)] = bloomskips; sv[Int(SFbloombits)] = bitcount(bloom_mask) end
                return i-n+1
            end # match at the very end
            j += 1
        end
    end
    if DOSTATS sv[Int(SFloops)] = loops end
    if DOSTATS sv[Int(SFbloomtests)] = bloomtests end
    if DOSTATS sv[Int(SFbloomskips)] = bloomskips end
    if DOSTATS sv[Int(SFbloombits)] = bitcount(bloom_mask) end
    0
end
