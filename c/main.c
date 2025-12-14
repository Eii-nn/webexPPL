#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <curl/curl.h>

// Callback function to write response data
size_t write_callback(void *contents, size_t size, size_t nmemb, void *userp) {
    (void)contents;
    (void)userp;
    return size * nmemb;
}

int main(int argc, char *argv[]) {
    // Initialize variables
    CURL *curl;
    CURLcode res;
    int i;
    int is_http;
    int is_https;
    clock_t start;
    clock_t end;
    double access_time;
    
    // Check if URL provided
    if (argc < 2) {
        printf("Usage: %s <url1> [url2] ...\n", argv[0]);
        return 1;
    }
    
    // Initialize curl
    curl_global_init(CURL_GLOBAL_DEFAULT);
    
    // Process each URL
    i = 1;
    while (i < argc) {
        // Check if URL starts with http://
        is_http = (strncmp(argv[i], "http://", 7) == 0);
        
        // Check if URL starts with https://
        is_https = (strncmp(argv[i], "https://", 8) == 0);
        
        // Validate protocol
        if (!is_http && !is_https) {
            printf("%s: Invalid URL (must start with http:// or https://)\n", argv[i]);
            i = i + 1;
            continue;
        }
        
        // Function 1: Validate HTTP/HTTPS (Safe or Not)
        printf("\nURL: %s\n", argv[i]);
        if (is_https) {
            printf("Status: SAFE (HTTPS - Encrypted)\n");
        } else {
            printf("Status: NOT SAFE (HTTP - Unencrypted)\n");
        }
        
        // Initialize curl handle
        curl = curl_easy_init();
        if (!curl) {
            fprintf(stderr, "Error: Failed to initialize curl\n");
            i = i + 1;
            continue;
        }
        
        // Function 2: Measure access time
        start = clock();
        
        // Configure curl
        curl_easy_setopt(curl, CURLOPT_URL, argv[i]);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
        curl_easy_setopt(curl, CURLOPT_NOBODY, 1L);
        curl_easy_setopt(curl, CURLOPT_TIMEOUT, 10L);
        curl_easy_setopt(curl, CURLOPT_CONNECTTIMEOUT, 5L);
        
        // Perform request
        res = curl_easy_perform(curl);
        
        end = clock();
        access_time = ((double)(end - start)) / CLOCKS_PER_SEC;
        
        // Display access time
        if (res == CURLE_OK) {
            printf("Access Time: %.3f seconds\n", access_time);
        } else {
            printf("Access Time: %.3f seconds (Failed: %s)\n", access_time, curl_easy_strerror(res));
        }
        
        // Cleanup
        curl_easy_cleanup(curl);
        
        i = i + 1;
    }
    
    // Cleanup curl
    curl_global_cleanup();
    
    return 0;
}
