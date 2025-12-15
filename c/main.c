#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <curl/curl.h>
#include <ctype.h>

struct MemoryStruct { char *memory; size_t size; };
struct Website { char *url; char *html_content; };
struct SafetyChecker { struct Website *website; int risk_score; char reasons[10][256]; int reason_count; };
struct PerformanceChecker { struct Website *website; };
struct AnalysisResult { char verdict[32]; int risk_score; char reasons[10][256]; int reason_count; double load_time; char performance[32]; };

size_t write_callback(void *contents, size_t size, size_t nmemb, void *userp) {
    size_t realsize = size * nmemb;
    struct MemoryStruct *mem = (struct MemoryStruct *)userp;
    char *ptr = realloc(mem->memory, mem->size + realsize + 1);
    if (!ptr) return 0;
    mem->memory = ptr;
    memcpy(&(mem->memory[mem->size]), contents, realsize);
    mem->size += realsize;
    mem->memory[mem->size] = 0;
    return realsize;
}

int website_is_https(struct Website *w) { return strncmp(w->url, "https://", 8) == 0; }
int website_length(struct Website *w) { return strlen(w->url); }
int website_has_suspicious_chars(struct Website *w) { return strchr(w->url, '@') != NULL; }
int website_has_many_dashes(struct Website *w) {
    int count = 0, i = 0;
    while (w->url[i] != '\0') if (w->url[i++] == '-') count++;
    return count > 3;
}
void website_fetch_html(struct Website *w) {
    CURL *curl;
    struct MemoryStruct chunk;
    chunk.memory = malloc(1);
    chunk.size = 0;
    curl = curl_easy_init();
    if (curl) {
        curl_easy_setopt(curl, CURLOPT_URL, w->url);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)&chunk);
        curl_easy_setopt(curl, CURLOPT_TIMEOUT, 5L);
        curl_easy_setopt(curl, CURLOPT_CONNECTTIMEOUT, 5L);
        if (curl_easy_perform(curl) == CURLE_OK && chunk.memory) {
            w->html_content = malloc(chunk.size + 1);
            size_t i = 0;
            while (i < chunk.size) {
                w->html_content[i] = tolower(chunk.memory[i]);
                i = i + 1;
            }
            w->html_content[chunk.size] = '\0';
        } else {
            w->html_content = malloc(1);
            w->html_content[0] = '\0';
        }
        curl_easy_cleanup(curl);
        free(chunk.memory);
    } else {
        w->html_content = malloc(1);
        w->html_content[0] = '\0';
    }
}

void safety_checker_check_https(struct SafetyChecker *c) {
    if (!website_is_https(c->website)) {
        c->risk_score += 30;
        strcpy(c->reasons[c->reason_count], "Website is not using HTTPS");
        c->reason_count = c->reason_count + 1;
    }
}
void safety_checker_check_url_length(struct SafetyChecker *c) {
    if (website_length(c->website) > 75) {
        c->risk_score += 10;
        strcpy(c->reasons[c->reason_count], "URL is unusually long");
        c->reason_count = c->reason_count + 1;
    }
}
void safety_checker_check_suspicious_characters(struct SafetyChecker *c) {
    if (website_has_suspicious_chars(c->website)) {
        c->risk_score += 25;
        strcpy(c->reasons[c->reason_count], "URL contains suspicious characters (@)");
        c->reason_count = c->reason_count + 1;
    }
}
void safety_checker_check_excessive_dashes(struct SafetyChecker *c) {
    if (website_has_many_dashes(c->website)) {
        c->risk_score += 15;
        strcpy(c->reasons[c->reason_count], "URL contains too many dashes");
        c->reason_count = c->reason_count + 1;
    }
}
void safety_checker_evaluate(struct SafetyChecker *c, struct AnalysisResult *r) {
    c->risk_score = c->reason_count = 0;
    safety_checker_check_https(c);
    safety_checker_check_url_length(c);
    safety_checker_check_suspicious_characters(c);
    safety_checker_check_excessive_dashes(c);
    website_fetch_html(c->website);
    if (c->risk_score >= 60) strcpy(r->verdict, "DANGEROUS");
    else if (c->risk_score >= 30) strcpy(r->verdict, "SUSPICIOUS");
    else strcpy(r->verdict, "SAFE");
    r->risk_score = c->risk_score;
    r->reason_count = c->reason_count;
    int i = 0;
    while (i < c->reason_count) {
        strcpy(r->reasons[i], c->reasons[i]);
        i = i + 1;
    }
}

double performance_checker_measure_load_time(struct PerformanceChecker *p) {
    CURL *curl;
    clock_t start = clock();
    struct MemoryStruct chunk;
    chunk.memory = malloc(1);
    chunk.size = 0;
    curl = curl_easy_init();
    if (curl) {
        curl_easy_setopt(curl, CURLOPT_URL, p->website->url);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)&chunk);
        curl_easy_setopt(curl, CURLOPT_TIMEOUT, 5L);
        curl_easy_setopt(curl, CURLOPT_CONNECTTIMEOUT, 5L);
        if (curl_easy_perform(curl) == CURLE_OK) {
            double load_time = ((double)(clock() - start)) / CLOCKS_PER_SEC;
            load_time = ((int)(load_time * 100 + 0.5)) / 100.0;
            curl_easy_cleanup(curl);
            free(chunk.memory);
            return load_time;
        }
        curl_easy_cleanup(curl);
    }
    free(chunk.memory);
    return -1.0;
}
void performance_checker_performance_rating(double load_time, char *rating) {
    if (load_time < 0) strcpy(rating, "Unreachable");
    else if (load_time < 1) strcpy(rating, "Fast");
    else if (load_time < 3) strcpy(rating, "Moderate");
    else strcpy(rating, "Slow");
}
void website_analyzer_analyze(struct Website *w, struct SafetyChecker *s, struct PerformanceChecker *p, struct AnalysisResult *r) {
    (void)w;
    safety_checker_evaluate(s, r);
    r->load_time = performance_checker_measure_load_time(p);
    performance_checker_performance_rating(r->load_time, r->performance);
}

int main(int argc, char *argv[]) {
    struct Website website;
    struct SafetyChecker safety_checker;
    struct PerformanceChecker perf_checker;
    struct AnalysisResult result;
    int i;
    if (argc < 2) {
        printf("Usage: %s <url1> [url2] ...\n", argv[0]);
        return 1;
    }
    curl_global_init(CURL_GLOBAL_DEFAULT);
    i = 1;
    while (i < argc) {
        if (strncmp(argv[i], "http://", 7) != 0 && strncmp(argv[i], "https://", 8) != 0) {
            printf("%s: Invalid URL\n", argv[i++]);
            continue;
        }
        website.url = argv[i];
        website.html_content = NULL;
        safety_checker.website = &website;
        perf_checker.website = &website;
        website_analyzer_analyze(&website, &safety_checker, &perf_checker, &result);
        printf("\n--- SAFETY ANALYSIS ---\n");
        printf("Risk Score: %d\n", result.risk_score);
        printf("Verdict: %s\n", result.verdict);
        if (result.reason_count > 0) {
            printf("Reasons:\n");
            int j = 0;
            while (j < result.reason_count) {
                printf("- %s\n", result.reasons[j]);
                j = j + 1;
            }
        } else {
            printf("No suspicious patterns detected.\n");
        }
        printf("\n--- PERFORMANCE ANALYSIS ---\n");
        if (result.load_time >= 0) {
            printf("Load Time: %.2f seconds\n", result.load_time);
            printf("Performance Rating: %s\n", result.performance);
        } else {
            printf("Website could not be reached.\n");
        }
        if (website.html_content) free(website.html_content);
        i++;
    }
    curl_global_cleanup();
    return 0;
}
