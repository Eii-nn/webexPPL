import java.net.HttpURLConnection;
import java.net.URL;
import java.io.IOException;

// Class to represent the result of checking a URL
class UrlResult {
    private String url;
    private boolean isSafe;
    private double accessTime;
    private boolean isSuccess;
    private String errorMessage;
    
    public UrlResult(String url, boolean isSafe, double accessTime, boolean isSuccess, String errorMessage) {
        this.url = url;
        this.isSafe = isSafe;
        this.accessTime = accessTime;
        this.isSuccess = isSuccess;
        this.errorMessage = errorMessage;
    }
    
    public String getUrl() {
        return url;
    }
    
    public boolean isSafe() {
        return isSafe;
    }
    
    public double getAccessTime() {
        return accessTime;
    }
    
    public boolean isSuccess() {
        return isSuccess;
    }
    
    public String getErrorMessage() {
        return errorMessage;
    }
    
    public void display() {
        System.out.println("\nURL: " + url);
        if (isSafe) {
            System.out.println("Status: SAFE (HTTPS - Encrypted)");
        } else {
            System.out.println("Status: NOT SAFE (HTTP - Unencrypted)");
        }
        
        if (isSuccess) {
            System.out.printf("Access Time: %.3f seconds\n", accessTime);
        } else {
            System.out.printf("Access Time: %.3f seconds (Failed: %s)\n", accessTime, errorMessage);
        }
    }
}

// Class to validate and check URLs
class UrlChecker {
    private static final int TIMEOUT = 10000; // 10 seconds
    private static final int CONNECT_TIMEOUT = 5000; // 5 seconds
    
    // Method to validate if URL is HTTP or HTTPS
    public boolean isValidProtocol(String url) {
        return url.startsWith("http://") || url.startsWith("https://");
    }
    
    // Method to check if URL is safe (HTTPS)
    public boolean isSafe(String url) {
        return url.startsWith("https://");
    }
    
    // Method to measure access time and check URL
    public UrlResult checkUrl(String urlString) {
        // Validate protocol
        if (!isValidProtocol(urlString)) {
            return new UrlResult(urlString, false, 0.0, false, 
                "Invalid URL (must start with http:// or https://)");
        }
        
        // Determine safety status
        boolean safe = isSafe(urlString);
        
        // Measure access time
        long startTime = System.currentTimeMillis();
        boolean success = false;
        String errorMessage = "";
        
        try {
            URL url = new URL(urlString);
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            connection.setRequestMethod("HEAD");
            connection.setConnectTimeout(CONNECT_TIMEOUT);
            connection.setReadTimeout(TIMEOUT);
            
            int responseCode = connection.getResponseCode();
            success = (responseCode >= 200 && responseCode < 400);
            
            connection.disconnect();
        } catch (IOException e) {
            errorMessage = e.getMessage();
            success = false;
        }
        
        long endTime = System.currentTimeMillis();
        double accessTime = (endTime - startTime) / 1000.0;
        
        return new UrlResult(urlString, safe, accessTime, success, errorMessage);
    }
}

// Main class to run the program
public class main {
    public static void main(String[] args) {
        // Check if URLs provided
        if (args.length == 0) {
            System.out.println("Usage: java main <url1> [url2] ...");
            System.exit(1);
        }
        
        // Create UrlChecker instance
        UrlChecker checker = new UrlChecker();
        
        // Process each URL
        for (String url : args) {
            UrlResult result = checker.checkUrl(url);
            result.display();
        }
    }
}
