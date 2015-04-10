type 'a t = 'a list

module Core : ListLike.CORE with type 'a t := 'a t
include ListLike.S with type 'a t := 'a t

