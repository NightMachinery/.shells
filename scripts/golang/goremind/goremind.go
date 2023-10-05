package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"time"
	// "strconv"

	"github.com/spf13/cobra"
	// "github.com/spf13/pflag"
	ptime "github.com/yaa110/go-persian-calendar"
)

func getJalaliMonthName(month int) string {
	monthNames := []string{
		"Farvardin", "Ordibehesht", "Khordad", "Tir", "Mordad", "Shahrivar",
		"Mehr", "Aban", "Azar", "Dey", "Bahman", "Esfand",
	}
	if month < 1 || month > 12 {
		return "Unknown"  // handle error or fallback case
	}
	return monthNames[month-1]
}

func remComingUp(remindersDir string, until int) string {
	today := time.Now()
	results := make([]string, until)

	var wg sync.WaitGroup
	for i := 1; i <= until; i++ {
		wg.Add(1)
		go func(i int) {
			defer wg.Done()

			futureDate := today.AddDate(0, 0, i)
			jalaliDate := ptime.New(futureDate)
			jalaliYear, jalaliMonth, jalaliDay := jalaliDate.Year(), jalaliDate.Month(), jalaliDate.Day()

			gregorianYear, gregorianMonth, gregorianDay := futureDate.Year(), futureDate.Month(), futureDate.Day()
			gregorianWeekDay := futureDate.Weekday().String()

			// Using the integer month value to get the string representation of the Jalali month
			// This assumes you have a function or map to convert the month number to its Jalali name.
			// If ptime provides this, you can use that. Otherwise, you might need to manually map month numbers to their names.
			jalaliMonthName := getJalaliMonthName(int(jalaliMonth))

			gregorianMonthName := gregorianMonth.String()

			futureText := fmt.Sprintf("%d day(s) later: %d/%s%d/%d %s %s%d/%d/%d",
				i,
				jalaliYear, jalaliMonthName, jalaliMonth, jalaliDay,
				gregorianWeekDay, gregorianMonthName, gregorianMonth, gregorianDay, gregorianYear)
			// futureText := fmt.Sprintf("%d day(s) later: %s", i, futureDate.Format("2006/01/02 (Mon)"))

			remToday := getRemToday(futureDate, remindersDir)
			if remToday != "" {
				futureText += "\n" + remToday
				results[i-1] = futureText
			}
		}(i)
	}
	wg.Wait()

	// Filter out empty results and return the joined string
	return strings.Join(filterEmpty(results), "\n")
}

func filterEmpty(strings []string) []string {
	var result []string
	for _, s := range strings {
		if s != "" {
			result = append(result, s)
		}
	}
	return result
}

func getRemToday(date time.Time, remindersDir string) string {
	jalaliDate := ptime.New(date)
	dateStr := fmt.Sprintf("%d/%02d/%02d", jalaliDate.Year(), jalaliDate.Month(), jalaliDate.Day())

	globPattern := filepath.Join(remindersDir, dateStr+"*")
	matches, _ := filepath.Glob(globPattern)

	if len(matches) > 0 {
		content, err := os.ReadFile(matches[0])
		if err != nil {
			return ""
		}
		return string(content)
	}

	return ""
}



func main() {
	var days int
	var remindersDir string

	// Define comingup as a subcommand.
	var comingupCmd = &cobra.Command{
		Use:   "comingup",
		Short: "Displays reminders for the upcoming days",
		Run: func(cmd *cobra.Command, args []string) {
			fmt.Println(remComingUp(remindersDir, days))
		},
	}

	comingupCmd.Flags().IntVarP(&days, "days", "d", 31, "Number of days to display reminders for")

	// Create the root command and add comingup as a subcommand.
	var rootCmd = &cobra.Command{Use: "goremind"}
	rootCmd.PersistentFlags().StringVarP(&remindersDir, "reminders-dir", "r", os.Getenv("remindayDir"),
		"Path to the reminders directory",
	)
	// I could not find the way to disable showing the default value in the help message,

	rootCmd.AddCommand(comingupCmd)

	if err := rootCmd.Execute(); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}
