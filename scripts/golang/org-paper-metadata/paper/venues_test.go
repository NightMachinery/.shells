package paper

import "testing"

func TestAdditionalVenueAbbreviations(t *testing.T) {
	for venue, want := range map[string]string{
		"European Conference on Artificial Intelligence":             "ECAI",
		"2024 European Conference on Artificial Intelligence (ECAI)": "ECAI",
		"  european   conference on artificial intelligence  ":       "ECAI",
		"ecai":                                "ECAI",
		"Asian Conference on Computer Vision": "ACCV",
		"AAAI/ACM Conference on AI, Ethics, and Society":                          "AIES",
		"ACM/AAAI Conference on Artificial Intelligence, Ethics and Society":      "AIES",
		"International Symposium on Intelligent Data Analysis":                    "IDA",
		"International Work-Conference on Artificial and Natural Neural Networks": "IWANN",
		"International Conference on System Science and Engineering":              "ICSSE",
		"IEEE Global Conference on Signal and Information Processing":             "GlobalSIP",
		"International Journal of Computer Vision":                                "IJCV",
		"Trans. Mach. Learn. Res.":                                                "TMLR",
		"Transactions on Machine Learning Research":                               "TMLR",
		"IJCAI-ECAI": "IJCAI/ECAI",
		"International Joint Conference on Artificial Intelligence and European Conference on Artificial Intelligence": "IJCAI/ECAI",
		"IEEE/CVF Conference on Computer Vision and Pattern Recognition Workshops":                                     "CVPRW",
		"Unknown Test Venue": "UnknownTestVenue",
	} {
		if got := VenueTag(venue); got != want {
			t.Errorf("%q: got %q, want %q", venue, got, want)
		}
	}
}
