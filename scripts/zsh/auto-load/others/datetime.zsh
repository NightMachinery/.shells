function fromnow() {
    mdoc "date= $0 -> seconds since \$date" MAGIC
    python3 -c 'import datetime ; from dateutil.parser import parse ; import os
date = parse(os.environ["date"])
print((datetime.datetime.now(date.tzinfo) - date).total_seconds())'
}
function dateshort() { date +"%b %d %H:%M:%S" }
dateshortnum() { date +"%Y/%m/%d" }
datej() { jalalim tojalali "$(dateshortnum)" }
