module Filter.Datatype
    ( Filter(..)
    )
where 

type Month = String

data Filter 
    = LatestByDate
    | ThisYearByMonth
    | BySubject

    | ThisSubject  String
    | ThisPage     Int
    | ThisAuthor   String
    | ThisCategory String
    | ThisMonth    Month
