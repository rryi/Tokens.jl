# Tree implementations, including TokenTree

"""
A tree having nodes of type NodeType.

Any subtype must implement the tree operations defined in this source file.
"""
abstract type Tree{NodeType}
end

"""
Tree implementation using indices into a node vector.

Instance vields are considered private, they must not be changed
outside the tree API, or tree corruption could happen.

idx stores the tree structure with the following conventions:

idx[n] == 0: marks the end of a subtree, no valid node index

idx[n]>0: base[idx[n]] is a leaf node in the tree

idx[n]<0: base[-idx[n]] is a node of a subtree having 0 or more children
idx[n+1]..idx[n+k] are the subtrees of all children (k=0 if no children present)
idx[n+k+1] is 0, marking the end if the list of children



"""
struct ITree{I<:Integer, NodeType} <: Tree{NodeType}
    idx ::Vector{I}
    base :: Vector{NodeType}
end


"""
A tree variant of TokenVector.

A TokenTree is a TokenVector organized as a tree.
In contrast to  but it offers an additional tree API.

A tree node is identified by an index in the token array.
Depending on its category, it is a leaf node (category <= T_END),
 or it has a (probably empty) subtree (category > T_END): its children are
the following nodes in the token array, until termination by a T_END token.
T_END tokens are no tree nodes not part of the tree structure, itThe index of a T_END token


"""
mutable struct TokenTree <: AbstractTokenVector{HybridFly}
    vec :: Vector{HybridFly}
    buffer :: String # private storage shared by all elements
    used :: UInt32 # no of bytes currently used
    shared :: UInt32 # no of bytes in buffer shared with other objects
    flags :: Int
end


# alternative: ein tree ist ein int32 array. Wert >0: leaf node.
# wert <0: subtree
# wert =0: end of subtree
#
# Vorteil: kann über JEDEM Vector gebaut werden (!!)
# vorteil: halbe größe der END marker.
# nachteil: zusätzliches array + 1 indirektion mehr
