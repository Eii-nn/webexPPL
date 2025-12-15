#pip install requests if wala pa na install

import time
import requests
from urllib.parse import urlparse


class Website:
    def __init__(self, url):
        self.url = url
        self.parsed_url = urlparse(url)
        self.html_content = None

    def is_https(self):
        return self.parsed_url.scheme == "https"

    def length(self):
        return len(self.url)

    def has_suspicious_chars(self):
        suspicious_symbols = ['@']
        return any(symbol in self.url for symbol in suspicious_symbols)

    def has_many_dashes(self):
        return self.url.count('-') > 3

    def fetch_html(self):
        try:
            response = requests.get(self.url, timeout=5)
            self.html_content = response.text.lower()
        except requests.exceptions.RequestException:
            self.html_content = ""


    def check_password_input(self):
        if self.website.html_content and "<input" in self.website.html_content:
            if 'type="password"' in self.website.html_content:
                if not self.website.is_https():
                    self.risk_score += 40
                    self.reasons.append(
                        "Password input detected on a non-HTTPS website"
                    )


class SafetyChecker:
    def __init__(self, website):
        self.website = website
        self.risk_score = 0
        self.reasons = []

    def check_https(self):
        if not self.website.is_https():
            self.risk_score += 30
            self.reasons.append("Website is not using HTTPS")

    def check_url_length(self):
        if self.website.length() > 75:
            self.risk_score += 10
            self.reasons.append("URL is unusually long")

    def check_suspicious_characters(self):
        if self.website.has_suspicious_chars():
            self.risk_score += 25
            self.reasons.append("URL contains suspicious characters (@)")

    def check_excessive_dashes(self):
        if self.website.has_many_dashes():
            self.risk_score += 15
            self.reasons.append("URL contains too many dashes")

    def evaluate(self):
        self.check_https()
        self.check_url_length()
        self.check_suspicious_characters()
        self.check_excessive_dashes()
        self.website.fetch_html()

        if self.risk_score >= 60:
            verdict = "DANGEROUS"
        elif self.risk_score >= 30:
            verdict = "SUSPICIOUS"
        else:
            verdict = "SAFE"

        return verdict, self.risk_score, self.reasons


class PerformanceChecker:
    def __init__(self, website):
        self.website = website

    def measure_load_time(self):
        try:
            start_time = time.time()
            requests.get(self.website.url, timeout=5)
            end_time = time.time()
            return round(end_time - start_time, 2)
        except requests.exceptions.RequestException:
            return None

    def performance_rating(self, load_time):
        if load_time is None:
            return "Unreachable"
        elif load_time < 1:
            return "Fast"
        elif load_time < 3:
            return "Moderate"
        else:
            return "Slow"


class WebsiteAnalyzer:
    def __init__(self, url):
        self.website = Website(url)
        self.safety_checker = SafetyChecker(self.website)
        self.performance_checker = PerformanceChecker(self.website)

    def analyze(self):
        verdict, score, reasons = self.safety_checker.evaluate()
        load_time = self.performance_checker.measure_load_time()
        performance = self.performance_checker.performance_rating(load_time)

        return {
            "verdict": verdict,
            "risk_score": score,
            "reasons": reasons,
            "load_time": load_time,
            "performance": performance
        }


#main program

if __name__ == "__main__":
    url = input("Enter URL: ")
    analyzer = WebsiteAnalyzer(url)
    result = analyzer.analyze()

    print("\n--- SAFETY ANALYSIS ---")
    print(f"Risk Score: {result['risk_score']}")
    print(f"Verdict: {result['verdict']}")

    if result["reasons"]:
        print("Reasons:")
        for reason in result["reasons"]:
            print(f"- {reason}")
    else:
        print("No suspicious patterns detected.")

    print("\n--- PERFORMANCE ANALYSIS ---")
    if result["load_time"] is not None:
        print(f"Load Time: {result['load_time']} seconds")
        print(f"Performance Rating: {result['performance']}")
    else:
        print("Website could not be reached.")
