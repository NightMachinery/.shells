#include <iostream>
#include <cstdio>
#include <string>
#include <boost/regex.hpp>
#include <iomanip>
#include <cstdlib>
#include <boost/locale.hpp>

using namespace std;

boost::locale::generator generator;
std::locale locale_gregorian = generator("en_US.UTF-8");
std::locale locale_jalali = generator("en_US.UTF-8@calendar=persian");

boost::locale::date_time jalali_to_gregorian(const int& year, const int& month, const int& day)
{
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
        << std::endl;
}

int main(int argc, char** argv) {
    std::istreambuf_iterator<char> begin{std::cin}, end;
    string s{begin, end};

    boost::regex root_heading_re{"^\\*\\s+"};
    boost::sregex_token_iterator root_heading_block_iter{s.begin(), s.end(), root_heading_re, -1}, regex_end{};
    // When class regex_token_iterator is used to enumerate a single sub-expression with index -1, then the iterator performs field splitting: that is to say it enumerates one character sequence for each section of the character container sequence that does not match the regular expression specified.

    boost::regex date_re{"(?:\\[|<)((?:jalali):([^]/]+)(?:/([^]/]+))?(?:/([^]/]+))?(?:/([^]]+))?)(?:\\]|>)",
    boost::regex::normal | boost::regbase::icase};

    vector<tuple<string, boost::locale::date_time>> blocks{};
    while (root_heading_block_iter!=regex_end) {
        const string& block_current = *root_heading_block_iter++;

        // cout << "Block:\n" << block_current << endl; // @ic

        const int subs[]{1,2,3,4};
        boost::sregex_token_iterator date_iter{block_current.begin(), block_current.end(), date_re, subs};

        vector<boost::locale::date_time> dates{};
        while (date_iter!=regex_end) {
            const string& link_str = *date_iter++;
            const string& year = *date_iter++;
            const string& month = *date_iter++;
            const string& day = *date_iter++;

            const int year_int = atoi(year.c_str());
            const int month_int = atoi(month.c_str());
            const int day_int = atoi(day.c_str());

            if (false) { // @ic
                cout << link_str << endl;

                cout << "year: " << year << endl;
                cout << "year_int: " << year_int << endl;
                cout << "month: " << month << endl;
                cout << "month_int: " << month_int << endl;
                cout << "day: " << day << endl;
                cout << "day_int: " << day_int << endl;
            }

            const boost::locale::date_time& date_norm = jalali_to_gregorian(year_int, month_int, day_int);

            dates.push_back(date_norm);
        }

        vector<boost::locale::date_time>::iterator selected_date_iter = min_element(dates.begin(), dates.end());
        if (selected_date_iter==dates.end()) {
            continue;
        } else {
            const auto& selected_date = *selected_date_iter;
            // date_print(selected_date); // @ic

            blocks.emplace_back(make_tuple(block_current, selected_date));
        }
    }

    sort(blocks.begin(), blocks.end(), [](tuple<string, boost::locale::date_time> a, tuple<string, boost::locale::date_time> b) {
        return get<1>(a) < get<1>(b);
    });

    for (const tuple<string, boost::locale::date_time>& block : blocks) {
        const string& block_str = get<0>(block);
        cout << "* " << block_str;
    }
    return 0;
}
