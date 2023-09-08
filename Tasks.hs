{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
module Tasks where


import Dataset
import Text.Read
import Text.Printf
import Data.List
import Data.Maybe

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]

compute_exam_grades :: Table -> Table
columns = ["Nume", "Punctaj Exam"]
columns2 = ["Q1", "Q2", "Q3", "Q4", "Q5", "Q6"]
columns3 = ["Q" , "0", "1", "2"]
columns4 = ["Nume", "Punctaj interviu", "Punctaj scris", "Diferenta"]

string_to_Float :: String -> Float
string_to_Float "" = 0.0
string_to_Float str = read str::Float

int_to_Float :: Int -> Float
int_to_Float x = fromIntegral x

rev_tail :: [a] -> [a]
rev_tail = (reverse.tail.reverse) 

rev_head :: [a] -> a
rev_head = (head.reverse)

both_tail :: [a] -> [a]
both_tail = (tail.rev_tail)

get_students_num [] = 0;
get_students_num (x:xs) = 1 + (get_students_num xs)

medie :: Row->Float
medie row = (foldr (+) 0.0 (map string_to_Float ((tail.rev_tail)row))) / 4

compute_exam_grades exam = columns:(calculate_grade (tail exam))
						where
							calculate_grade :: Table -> Table
							calculate_grade [] = []
							calculate_grade (x:xs) = (calculate_grade_lin x):(calculate_grade xs)
								where
									calculate_grade_lin :: Row -> Row
									calculate_grade_lin row = (head row): [(printf "%.2f") ((string_to_Float (rev_head row)) + (medie row))]

get_passed_students_num :: Table -> Int
get_passed_students_num exam = count (tail (compute_exam_grades exam))
						where
							count [] = 0
							count (x:xs) = if ((string_to_Float (x !! 1)) >= 2.5) then 1 + (count xs) else (count xs)

get_passed_students_percentage :: Table -> Float
get_passed_students_percentage exam = (int_to_Float (get_passed_students_num exam)) / (int_to_Float (get_students_num (tail exam)))

get_exam_avg :: Table -> Float
get_exam_avg exam = (foldl (+) 0.0 (list_of_grades exam)) / (int_to_Float (length (list_of_grades exam)))
					where
						list_of_grades exam = (map string_to_Float (map (!!1) (tail (compute_exam_grades exam))))

compute_hw_table :: Table -> [Float]
compute_hw_table [] = []
compute_hw_table (x:xs) = (foldl (+) 0.0 (map string_to_Float ([x!!2] ++ [x!!3] ++ [x!!4]))) :(compute_hw_table xs)

get_passed_hw_num :: Table -> Int
get_passed_hw_num hw = length (filter (>= 1.5) (compute_hw_table (tail hw)))
						

get_avg_responses_per_qs :: Table -> Table
get_avg_responses_per_qs exam = columns2:[(map (printf "%.2f") (parse_responses (transpose (tail (map (both_tail) exam)))))]
					where
						parse_responses [] = []
						parse_responses (x:xs) = ((foldl (+) 0 (map string_to_Float x)) / (get_students_num (tail exam))):(parse_responses xs)
get_exam_summary :: Table -> Table
get_exam_summary exam = columns3:(parse_responses 0 (transpose (tail (map (both_tail) exam))))
					where
						parse_responses num [] = []
						parse_responses num (x:xs) =  ((columns2!!num):(map (printf "%.2d") ((length (filter(\y->if y == "0" || y == "" then True else False) x)):(length (filter(=="1") x)):(length (filter(=="2") x)):[]))):(parse_responses (num + 1) xs)

myCompare :: Row->Row->Ordering
myCompare r1 r2 = if (grade1 > grade2) then GT else if (grade1 < grade2) then LT else if name1 > name2 then GT else if name1 < name2 then LT else EQ
					where
						grade1 = (string_to_Float(r1!!1))
						grade2 = (string_to_Float(r2!!1))
						name1 = r1!!0
						name2 = r2!!0

get_ranking :: Table -> Table
get_ranking exam = columns:(sortBy myCompare (tail (compute_exam_grades exam)))

compute_final_table exam = columns4:(calculate_grade (tail exam))
						where
							calculate_grade :: Table -> Table
							calculate_grade [] = []
							calculate_grade (x:xs) = (calculate_grade_lin x):(calculate_grade xs)
								where
									calculate_grade_lin :: Row -> Row
									calculate_grade_lin row = (myList row) ++ [(printf "%.2f") (abs((string_to_Float((myList row)!!1)) - (string_to_Float((myList row)!!2))))]
										where
											myList row = (head row): ((printf "%.2f") (medie row)):((printf "%.2f") ((string_to_Float (rev_head row)))) :[]

myCompare2 :: Row->Row->Ordering
myCompare2 r1 r2 = if (dif1 > dif2) then GT else if (dif1 < dif2) then LT else if name1 > name2 then GT else if name1 < name2 then LT else EQ
					where
						dif1 = (string_to_Float(r1!!3))
						dif2 = (string_to_Float(r2!!3))
						name1 = r1!!0
						name2 = r2!!0


get_exam_diff_table :: Table -> Table
get_exam_diff_table exam = columns4:(sortBy myCompare2 (tail (compute_final_table exam)))


---Etapa 2
-- Face split dupa virgula
removeComma :: String -> [String]
removeComma = foldr (\letter acc -> if letter == ',' then []:acc else (letter:(head acc)):(tail acc)) [[]]

-- Citeste un fisier CSV
read_csv :: CSV -> Table
read_csv = \csv -> map removeComma (lines csv)

-- Scrie un fisier CSV pornind de la un table
write_csv :: Table -> CSV
write_csv = \table -> intercalate "\n" (map (intercalate ",") table)

-- extrage coloana ceruta
as_list :: String -> Table -> [String]
as_list s t = tail $ head $ filter(\r -> (head r) == s) (transpose t)

tsort :: String -> Table -> Table
tsort = \s -> \t -> (head t) : (sortBy (sort_fct (head t) s) (tail t)) where
	sort_fct :: Row -> String -> Row -> Row -> Ordering
	sort_fct hdr s = \r1 -> \r2 -> if compare (find_col hdr r1 s) (find_col hdr r2 s) /= EQ then compare (find_col hdr r1 s) (find_col hdr r2 s)
										else compare (head r1) (head r2)
		where
			find_col :: Row -> Row -> String -> String
			find_col (x:xs) (y:ys) name
				| x == name = y
				| otherwise = find_col xs ys name
-- aplica f pe toate valorile din tabel
vmap :: (Value -> Value) -> Table -> Table
vmap f = map (\r -> (map f r))

-- aplica f pe toate liniile din tabel
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f cols (x:xs) = [cols] ++ (map f xs)

-- extrage totalul temelor din tabel, alaturi de numele persoanei
get_hw_grade_total :: Row -> Row
get_hw_grade_total (x:y:xs) = x:((printf "%.2f") (sum $ map string_to_Float xs)):[]

-- daca coincid numele coloanelor, realizez concatenare, altfel intorc prima tabela
vunion :: Table -> Table -> Table
vunion tabel1 tabel2
	| (head tabel1) == (head tabel2) = tabel1 ++ (tail tabel2)
	| otherwise = tabel1

add_to_right :: Table -> Table -> Row -> Row
add_to_right t1 t2 row_from_t1
	| (fromJust (elemIndex row_from_t1 t1)) >= length t2 = row_from_t1 ++ (take (length $ head t2) (repeat "")) 
	| otherwise = row_from_t1 ++ (t2 !! (fromJust (elemIndex row_from_t1 t1)))

add_to_left :: Table -> Table -> Row -> Row
add_to_left t1 t2 row_from_t2
	| (fromJust (elemIndex row_from_t2 t2)) >= length t1 = (take (length $ head t1) (repeat "")) ++ row_from_t2 
	| otherwise = (t1 !! (fromJust (elemIndex row_from_t2 t2))) ++ row_from_t2

hunion :: Table -> Table -> Table
hunion t1 t2
	| (length t1) > (length t2) = map (add_to_right t1 t2) t1
	| otherwise = map (add_to_left t1 t2) t2

elemIndexLast :: Eq a => a -> [a] -> Maybe Int
elemIndexLast el list 
	| isNothing (elemIndex el list) = Nothing
	| otherwise = Just ((length list) - (fromJust (elemIndex el (reverse list))))

remlastkey :: String -> Table -> Table
remlastkey s tabel =  transpose ((take (pos - 1) (transpose tabel)) ++ (drop pos (transpose tabel)))
		where pos = (fromJust (elemIndexLast s (head tabel)))

tjoin :: String -> Table -> Table -> Table
tjoin s t1 t2  = remlastkey s (foldr (merger (fromJust (elemIndex s (head t1))) (fromJust (elemIndex s (head t2))) t2) [] t1) where
	merger :: Int -> Int -> Table -> Row -> Table -> Table
	merger pos1 pos2 t2 row acc
		| filter (\row2 -> row2!!pos2 == row!!pos1) t2 == [] = (row ++ ((take (length $ head t2)) (repeat ""))):acc
		| otherwise = (row ++ (head (filter (\row2 -> row2!!pos2 == row!!pos1) t2))):acc

cartesian :: (Row -> Row -> Row) -> [String] -> Table -> Table -> Table
cartesian func new_hdr t1 t2 = new_hdr:[(func r1 r2) | r1 <- (tail t1),
													   r2 <- (tail t2)]

{- transpun pt a lucra mai usor cu matricea, si iau doar entry urile ce au 
numele coloanei apartinand primului parametru -}
projection :: [String] -> Table -> Table
projection s t1 = transpose $ filter (\r -> elem (head r) s) (transpose t1)

type FilterOp = Row -> Bool

data FilterCondition a =
    Eq String a |
	Diff String a |
	Lt String a |
    Gt String a |
    In String [a] |
	NotIn String [a] |
	FieldDiff String String |
    FNot (FilterCondition a) |
    FieldEq String String

data QResult = CSV CSV | Table Table | List [String]

data Query =
    FromCSV CSV
    | ToCSV Query
    | AsList String Query
    | Sort String Query
    | ValueMap (Value -> Value) Query
    | RowMap (Row -> Row) [String] Query
    | VUnion Query Query
    | HUnion Query Query
    | TableJoin String Query Query
    | Cartesian (Row -> Row -> Row) [String] Query Query
    | Projection [String] Query
    | forall a. FEval a => Filter (FilterCondition a) Query
    | Graph EdgeOp Query
-- where EdgeOp is defined:
type EdgeOp = Row -> Row -> Maybe Value

class FEval a where
	feval :: [String] -> (FilterCondition a) -> FilterOp

class Eval a where
	eval :: a -> QResult

convert :: QResult -> Maybe Table
convert (CSV c) = Just (read_csv c)
convert (Table tabel) = Just tabel
convert (List l) = Nothing

final_table :: Query -> Table
final_table query 
	| isJust (convert (eval query)) = fromJust (convert (eval query))
	| otherwise = []

findVal :: Row -> String -> Int -> Int
findVal (x:xs) col acc
	| x == col = acc
	| otherwise = findVal xs col (acc + 1)

instance Eval Query where
	eval (Filter op query) = Table ([head (final_table query)] ++ (filter (feval (head (final_table query)) op) (tail (final_table query))))
	eval (FromCSV str) = Table (read_csv str) 
	eval (ToCSV query) =  CSV (write_csv (final_table query))
	eval (AsList colname query) = List (as_list colname (final_table query))
	eval (ValueMap op query) = Table (vmap op (final_table query))
	eval (Sort colname query) = Table (tsort colname (final_table query))
	eval (RowMap op colnames query) = Table (rmap op colnames (final_table query))
	eval (HUnion query1 query2) = Table (hunion (final_table query1) (final_table query2)) 
	eval (VUnion query1 query2) = Table (vunion (final_table query1) (final_table query2)) 
	eval (Projection colnames query) = Table (projection colnames (final_table query))
	eval (TableJoin colname query1 query2) = Table (tjoin colname (final_table query1) (final_table query2))
	eval (Cartesian op colnames query1 query2) = Table (cartesian op colnames (final_table query1) (final_table query2))

instance Show QResult where
	show (List l) = show l
	show (Table tabel) = write_csv tabel
	show (CSV csv) = show csv

instance FEval String where
	feval hdr (Eq col val) r = val == (r!!(findVal hdr col 0))
	feval hdr (Diff col val) r = val /=  (r!!(findVal hdr col 0))
	feval hdr (Lt col val) r = val > (r!!(findVal hdr col 0))
	feval hdr (Gt col val) r = val <  (r!!(findVal hdr col 0))
	feval hdr (In col list) r = if elem ((r!!(findVal hdr col 0))) list then True
									else False
	feval hdr (NotIn col list) r = if elem ((r!!(findVal hdr col 0))) list then False
									else True
	feval hdr (FieldEq col1 col2) r = ((r!!(findVal hdr col2 0))) == ((r!!(findVal hdr col1 0)))
	feval hdr (FieldDiff col1 col2) r = ((r!!(findVal hdr col2 0))) /= ((r!!(findVal hdr col1 0)))

	feval hdr (FNot fcond) r = if (feval hdr fcond r) == True then False else True


instance FEval Float where
	feval hdr (Eq col val) r = val == string_to_Float (r!!(findVal hdr col 0))
	feval hdr (Diff col val) r = val /= string_to_Float (r!!(findVal hdr col 0))
	feval hdr (Lt col val) r = val > string_to_Float (r!!(findVal hdr col 0))
	feval hdr (Gt col val) r = val < string_to_Float (r!!(findVal hdr col 0))
	feval hdr (In col list) r = if elem (string_to_Float(r!!(findVal hdr col 0))) list then True
									else False
	feval hdr (NotIn col list) r = if elem (string_to_Float(r!!(findVal hdr col 0))) list then False
									else True
	feval hdr (FieldEq col1 col2) r = (string_to_Float(r!!(findVal hdr col2 0))) == (string_to_Float(r!!(findVal hdr col1 0)))
	feval hdr (FieldDiff col1 col2) r = (string_to_Float(r!!(findVal hdr col2 0))) /= (string_to_Float(r!!(findVal hdr col1 0)))

	feval hdr (FNot fcond) r = if (feval hdr fcond r) == True then False else True

similarities_query = FromCSV exam_grades_csv
