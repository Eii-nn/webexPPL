#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <curl/curl.h>

// Structure to hold response data
struct MemoryStruct {
    char *memory;
    size_t size;
};

// Callback function to write data from curl
static size_t WriteMemoryCallback(void *contents, size_t size, size_t nmemb, void *userp) {
    size_t realsize = size * nmemb;
    struct MemoryStruct *mem = (struct MemoryStruct *)userp;
    
    char *ptr = realloc(mem->memory, mem->size + realsize + 1);
    if (ptr == NULL) {
        printf("Not enough memory (realloc returned NULL)\n");
        return 0;
    }
    
    mem->memory = ptr;
    memcpy(&(mem->memory[mem->size]), contents, realsize);
    mem->size += realsize;
    mem->memory[mem->size] = 0;
    
    return realsize;
}

// Function to check if URL is HTTPS (basic safety check)
int is_https(const char *url) {
    return strncmp(url, "https://", 8) == 0;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Usage: %s <URL>\n", argv[0]);
        return 1;
    }
    
    const char *url = argv[1];
    CURL *curl;
    CURLcode res;
    struct MemoryStruct chunk;
    chunk.memory = malloc(1);
    chunk.size = 0;
    
    clock_t start, end;
    double cpu_time_used;
    
    // Initialize curl
    curl = curl_easy_init();
    if (curl) {
        // Set URL
        curl_easy_setopt(curl, CURLOPT_URL, url);
        
        // Follow redirects
        curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
        
        // Set write callback
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteMemoryCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)&chunk);
        
        // Start timing
        start = clock();
        
        // Perform the request
        res = curl_easy_perform(curl);
        
        // End timing
        end = clock();
        cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
        
        if (res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
        } else {
            // Performance check: Response time
            printf("Performance Check:\n");
            printf("Response time: %.2f seconds\n", cpu_time_used);
            
            // Basic safety checks
            printf("\nSafety Check:\n");
            if (is_https(url)) {
                printf("Uses HTTPS: Yes\n");
            } else {
                printf("Uses HTTPS: No (Warning: Not secure)\n");
            }
            
            if (has_malicious_content(chunk.memory)) {
                printf("Potential malicious content detected: Yes (Warning)\n");
            } else {
                printf("Potential malicious content detected: No\n");
            }
            
            // Get HTTP response code
            long response_code;
            curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &response_code);
            printf("HTTP Status Code: %ld\n", response_code);
        }
        
        // Cleanup
        curl_easy_cleanup(curl);
    }
    
    free(chunk.memory);
    return 0;
}

