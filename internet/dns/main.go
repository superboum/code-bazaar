package main

import(
	"fmt"
	"io"
	"log"
	"bufio"
	"strings"
	"net/http"
)

func main() {
	url := "https://data.iana.org/TLD/tlds-alpha-by-domain.txt"


	resp, err := http.Get(url)
	if err != nil {
		log.Fatal(err)	
	}

	tld_count := 0
	reader := bufio.NewReader(resp.Body)
	for {
		buf, err := reader.ReadBytes('\n')
		if err == io.EOF {
			break
		} else if err != nil {
			log.Fatal(err)	
		}
		line := string(buf)
		if strings.HasPrefix(line, "#") {
			continue
		}

		fmt.Print(line)
		tld_count += 1
    	}
	fmt.Printf("count: %d TLDs\n", tld_count)
}
