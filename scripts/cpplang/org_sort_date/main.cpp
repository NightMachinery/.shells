#include <cstring>
#include <iostream>
#include <cstdio>
#include <string>
#include <boost/regex.hpp>
#include <iomanip>
#include <cstdlib>
#include <boost/locale.hpp>

using namespace std;

boost::locale::date_time from_jalali_to_gregorian(const int& year, const int& month, const int& day)
{
    boost::locale::generator generator;

    std::locale locale_gregorian = generator("en_US.UTF-8");
    std::locale locale_jalali = generator("en_US.UTF-8@calendar=persian");

    boost::locale::date_time jalali(
        boost::locale::period::year(year)
        + boost::locale::period::month(month - 1) // @containedSurprise months are zero-based
        + boost::locale::period::day(day),
        locale_jalali);

    boost::locale::date_time gregorian(jalali.time(), locale_gregorian);

    return gregorian;
}

void date_print(const boost::locale::date_time& date) {
    std::cout
              << (date.get(boost::locale::period::year()))
              << "/" << std::setfill('0') << std::setw(2)
              << (date.get(boost::locale::period::month()) + 1)
              << "/" << std::setfill('0') << std::setw(2)
              << (date.get(boost::locale::period::day()))
              << "." << std::endl;
}

int main(int argc, char** argv) {
    std::istreambuf_iterator<char> begin{std::cin}, end;
    string s{begin, end};

    boost::regex root_heading_re{"^\\*\\s+"};
    boost::sregex_token_iterator root_heading_block{s.begin(), s.end(), root_heading_re, -1}, regex_end{};
    // When class regex_token_iterator is used to enumerate a single sub-expression with index -1, then the iterator performs field splitting: that is to say it enumerates one character sequence for each section of the character container sequence that does not match the regular expression specified.

    boost::regex date_re{"(?:\\[|<)((?:jalali):([^]/]+)(?:/([^]/]+))?(?:/([^]/]+))?(?:/([^]]+))?)(?:\\]|>)",
    boost::regex::normal | boost::regbase::icase};

    while (root_heading_block!=regex_end) {
        const string& found = *root_heading_block++;

        // cout << "Block:\n" << found << endl; // @ic

        const int subs[]{1,2,3,4};
        boost::sregex_token_iterator date_iter{found.begin(), found.end(), date_re, subs};
        while (date_iter!=regex_end) {
            const string& date = *date_iter++;
            const string& year = *date_iter++;
            const string& month = *date_iter++;
            const string& day = *date_iter++;

            const int year_int = atoi(year.c_str());
            const int month_int = atoi(month.c_str());
            const int day_int = atoi(day.c_str());

            cout << date << endl;
            cout << "year: " << year << endl;
            cout << "year_int: " << year_int << endl;
            cout << "month: " << month << endl;
            cout << "month_int: " << month_int << endl;
            cout << "day: " << day << endl;
            cout << "day_int: " << day_int << endl;

            const boost::locale::date_time& date_norm = from_jalali_to_gregorian(year_int, month_int, day_int);

            date_print(date_norm);
        }
    }

    return 0;
}
