fun is_older (date1 : int*int*int, date2 : int*int*int) =
    let 
        val year1 = #1 date1
        val month1 = #2 date1
        val day1 = #3 date1
        val year2 = #1 date2
        val month2 = #2 date2
        val day2 = #3 date2
    in
        year1 < year2 orelse (year1=year2 andalso month1 < month2)
                orelse (year1=year2 andalso month1=month2 andalso day1 < day2)
    end 

fun number_in_month(dates: (int*int*int) list, month: int) : int =
    if null dates
    then 0
    else
        let
            val tl_ans = tl dates
            val date = hd dates
        in
            if #2 (date) = month
            then 1 + number_in_month(tl_ans, month)
            else 0 + number_in_month(tl_ans, month)
        end

fun number_in_months(dates: (int*int*int) list, months: int list): int =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates: (int*int*int) list, month: int) : (int*int*int) list =
    if null dates
    then []
    else
        let 
            val tl_ans = tl dates
            val date = hd dates
        in
            if #2 date = month
            then date::dates_in_month(tl_ans, month)
            else dates_in_month(tl_ans, month)
        end

fun dates_in_months(dates: (int*int*int) list, months: int list) : (int*int*int) list =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months) 

fun get_nth(strings: string list, n: int) : string =
    if n = 1
    then hd strings
    else
        get_nth(tl strings, n - 1)

fun date_to_string(date: int*int*int) : string =
    let
        val months = "January"::"February"::"March"::"April"::"May"::"June"::"July"::"August"::"September"::"October"::"November"::"December"::[]
        val month = get_nth(months, #2 date)
    in
        month ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

fun number_before_reaching_sum(sum: int, numbers: int list) : int =
    if sum <= hd numbers
    then 0
    else 1 + number_before_reaching_sum(sum - hd numbers, tl numbers)

fun what_month(day: int) : int = 
    let
        val days_of_month = 31::28::31::30::31::30::31::31::30::31::30::31::[]
    in
        1 + number_before_reaching_sum(day, days_of_month)
    end

fun month_range(day1: int, day2: int) : int list =
    if day1 > day2
    then []
    else what_month(day1)::month_range(day1 + 1, day2)

fun oldest(dates: (int*int*int) list) : (int*int*int) option =
    if null dates
    then NONE
    else if null (tl dates)
        then SOME(hd dates)
        else let 
                val tl_ans = oldest(tl dates)
            in
                if isSome(tl_ans)
                    then if is_older(hd dates, valOf(tl_ans))
                        then SOME(hd dates)
                        else tl_ans
                else SOME(hd dates)
            end