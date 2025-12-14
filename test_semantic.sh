#!/bin/bash

# Enhanced Semantic Analysis Test Script for MiniC Compiler

# Set Java 25 if available
if /usr/libexec/java_home -v 25 &>/dev/null; then
    export JAVA_HOME=$(/usr/libexec/java_home -v 25)
    export PATH="$JAVA_HOME/bin:$PATH"
fi
# Tests all semantic analysis test cases and provides detailed summary
# Runs in a loop with interactive prompts and shows differences between runs

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
NC='\033[0m' # No Color

# Directories
TESTCASES_DIR="minic/semanticanalysis/tst/base/testcases"
SOLUTIONS_DIR="minic/semanticanalysis/tst/base/solutions"
JAR_FILE="build/libs/MiniC-SemAnalysis.jar"
TEMP_DIR="./minic_sem_test_$$"
RESULTS_DIR="./minic_sem_results_$$"

# Create directories for storing results
mkdir -p "$TEMP_DIR"
mkdir -p "$RESULTS_DIR"

# Variables for tracking previous run
PREV_PASSED_TESTS=0
PREV_FAILED_TESTS=0
PREV_PERCENTAGE=0
declare -a PREV_FAILED_LIST
RUN_COUNT=0

# Function to run tests
run_tests() {
    # Counters
    TOTAL_TESTS=0
    PASSED_TESTS=0
    FAILED_TESTS=0
    BONUS_TESTS=0
    BONUS_PASSED=0

    # Array to store failed test details
    declare -a FAILED_LIST

    RUN_COUNT=$((RUN_COUNT + 1))

    echo -e "${BLUE}========================================${NC}"
    echo -e "${BLUE}  MiniC Semantic Analysis Test Suite (Run #${RUN_COUNT})${NC}"
    echo -e "${BLUE}========================================${NC}"
    echo ""

    # Step 1: Build the project
    echo -e "${YELLOW}Building project...${NC}"
    ./gradlew jar -q > /dev/null 2>&1

    if [ $? -ne 0 ]; then
        echo -e "${RED}Build failed! Please fix compilation errors first.${NC}"
        return 1
    fi

    if [ ! -f "$JAR_FILE" ]; then
        echo -e "${RED}JAR file not found: $JAR_FILE${NC}"
        echo -e "${YELLOW}Try running: ./gradlew jarNoScannerNoParser${NC}"
        return 1
    fi

    echo -e "${GREEN}Build successful!${NC}"
    echo ""

    # Step 2: Run all test cases
    echo -e "${YELLOW}Running tests...${NC}"
    echo ""

    for testfile in "$TESTCASES_DIR"/*.mc; do
        if [ -f "$testfile" ]; then
            # Extract test name (e.g., c1.mc, c15.1.mc, etc.)
            filename=$(basename "$testfile")
            testname="${filename%.mc}"

            TOTAL_TESTS=$((TOTAL_TESTS + 1))

            # Check if this is a bonus test (c27 and above)
            testnum=$(echo "$testname" | sed 's/c\([0-9]*\).*/\1/')
            is_bonus=0
            if [ "$testnum" -ge 27 ] 2>/dev/null; then
                is_bonus=1
                BONUS_TESTS=$((BONUS_TESTS + 1))
            fi

            # Run the semantic analyzer
            output_file="$TEMP_DIR/output_${testname}.txt"
            java -ea -jar "$JAR_FILE" "$testfile" > "$output_file" 2>&1

            # Check if output file was created
            if [ ! -f "$output_file" ]; then
                echo -e "  ${RED}✗${NC} Test ${testname}: Output file not created"
                FAILED_TESTS=$((FAILED_TESTS + 1))
                FAILED_LIST+=("${testname}")
                continue
            fi

            # Compare with expected solution
            solution_file="$SOLUTIONS_DIR/${testname}.mc.sol"

            if [ ! -f "$solution_file" ]; then
                echo -e "  ${YELLOW}?${NC} Test ${testname}: Solution file not found (skipping)"
                continue
            fi

            # Compare output (ignore trailing whitespace)
            diff -u --ignore-all-space "$solution_file" "$output_file" > /dev/null 2>&1

            if [ $? -eq 0 ]; then
                if [ $is_bonus -eq 1 ]; then
                    echo -e "  ${GREEN}✓${NC} Test ${testname}: PASSED ${CYAN}(BONUS)${NC}"
                    BONUS_PASSED=$((BONUS_PASSED + 1))
                else
                    echo -e "  ${GREEN}✓${NC} Test ${testname}: PASSED"
                fi
                PASSED_TESTS=$((PASSED_TESTS + 1))
            else
                if [ $is_bonus -eq 1 ]; then
                    echo -e "  ${RED}✗${NC} Test ${testname}: FAILED ${CYAN}(BONUS)${NC}"
                else
                    echo -e "  ${RED}✗${NC} Test ${testname}: FAILED"
                fi
                FAILED_TESTS=$((FAILED_TESTS + 1))
                FAILED_LIST+=("${testname}")

                # Save diff for debugging
                diff -u --ignore-all-space "$solution_file" "$output_file" > "$RESULTS_DIR/diff_${testname}.txt" 2>&1
            fi
        fi
    done

    # Calculate percentages
    if [ $TOTAL_TESTS -gt 0 ]; then
        PERCENTAGE=$((PASSED_TESTS * 100 / TOTAL_TESTS))
    else
        PERCENTAGE=0
    fi

    CORE_TESTS=$((TOTAL_TESTS - BONUS_TESTS))
    CORE_PASSED=$((PASSED_TESTS - BONUS_PASSED))
    if [ $CORE_TESTS -gt 0 ]; then
        CORE_PERCENTAGE=$((CORE_PASSED * 100 / CORE_TESTS))
    else
        CORE_PERCENTAGE=0
    fi

    if [ $BONUS_TESTS -gt 0 ]; then
        BONUS_PERCENTAGE=$((BONUS_PASSED * 100 / BONUS_TESTS))
    else
        BONUS_PERCENTAGE=0
    fi

    # Step 3: Display summary
    echo ""
    echo -e "${BLUE}========================================${NC}"
    echo -e "${BLUE}  Test Summary${NC}"
    echo -e "${BLUE}========================================${NC}"
    echo ""
    echo -e "${MAGENTA}Overall:${NC}"
    echo -e "  Total Tests:   ${BLUE}${TOTAL_TESTS}${NC}"
    echo -e "  Passed:        ${GREEN}${PASSED_TESTS}${NC}"
    echo -e "  Failed:        ${RED}${FAILED_TESTS}${NC}"
    echo -e "  Success Rate:  ${BLUE}${PERCENTAGE}%${NC}"
    echo ""
    echo -e "${MAGENTA}Core Tests (Required):${NC}"
    echo -e "  Tests:         ${BLUE}${CORE_TESTS}${NC}"
    echo -e "  Passed:        ${GREEN}${CORE_PASSED}${NC}"
    echo -e "  Success Rate:  ${BLUE}${CORE_PERCENTAGE}%${NC}"
    echo ""
    if [ $BONUS_TESTS -gt 0 ]; then
        echo -e "${CYAN}Bonus Tests (Arrays - 10% extra):${NC}"
        echo -e "  Tests:         ${BLUE}${BONUS_TESTS}${NC}"
        echo -e "  Passed:        ${GREEN}${BONUS_PASSED}${NC}"
        echo -e "  Success Rate:  ${CYAN}${BONUS_PERCENTAGE}%${NC}"
        echo ""
    fi

    # Display differences from previous run
    if [ $RUN_COUNT -gt 1 ]; then
        echo -e "${CYAN}========================================${NC}"
        echo -e "${CYAN}  Comparison with Previous Run${NC}"
        echo -e "${CYAN}========================================${NC}"
        echo ""

        # Calculate differences
        PASSED_DIFF=$((PASSED_TESTS - PREV_PASSED_TESTS))
        FAILED_DIFF=$((FAILED_TESTS - PREV_FAILED_TESTS))
        PERCENTAGE_DIFF=$((PERCENTAGE - PREV_PERCENTAGE))

        # Display changes
        if [ $PASSED_DIFF -gt 0 ]; then
            echo -e "Passed:        ${GREEN}+${PASSED_DIFF}${NC} (${PREV_PASSED_TESTS} → ${PASSED_TESTS})"
        elif [ $PASSED_DIFF -lt 0 ]; then
            echo -e "Passed:        ${RED}${PASSED_DIFF}${NC} (${PREV_PASSED_TESTS} → ${PASSED_TESTS})"
        else
            echo -e "Passed:        ${NC}No change (${PASSED_TESTS})${NC}"
        fi

        if [ $FAILED_DIFF -gt 0 ]; then
            echo -e "Failed:        ${RED}+${FAILED_DIFF}${NC} (${PREV_FAILED_TESTS} → ${FAILED_TESTS})"
        elif [ $FAILED_DIFF -lt 0 ]; then
            echo -e "Failed:        ${GREEN}${FAILED_DIFF}${NC} (${PREV_FAILED_TESTS} → ${FAILED_TESTS})"
        else
            echo -e "Failed:        ${NC}No change (${FAILED_TESTS})${NC}"
        fi

        if [ $PERCENTAGE_DIFF -gt 0 ]; then
            echo -e "Success Rate:  ${GREEN}+${PERCENTAGE_DIFF}%${NC} (${PREV_PERCENTAGE}% → ${PERCENTAGE}%)"
        elif [ $PERCENTAGE_DIFF -lt 0 ]; then
            echo -e "Success Rate:  ${RED}${PERCENTAGE_DIFF}%${NC} (${PREV_PERCENTAGE}% → ${PERCENTAGE}%)"
        else
            echo -e "Success Rate:  ${NC}No change (${PERCENTAGE}%)${NC}"
        fi
        echo ""

        # Show newly fixed tests
        declare -a NEWLY_FIXED
        for prev_fail in "${PREV_FAILED_LIST[@]}"; do
            is_still_failing=0
            for curr_fail in "${FAILED_LIST[@]}"; do
                if [ "$prev_fail" == "$curr_fail" ]; then
                    is_still_failing=1
                    break
                fi
            done
            if [ $is_still_failing -eq 0 ]; then
                NEWLY_FIXED+=("$prev_fail")
            fi
        done

        # Show newly broken tests
        declare -a NEWLY_BROKEN
        for curr_fail in "${FAILED_LIST[@]}"; do
            is_new=1
            for prev_fail in "${PREV_FAILED_LIST[@]}"; do
                if [ "$curr_fail" == "$prev_fail" ]; then
                    is_new=0
                    break
                fi
            done
            if [ $is_new -eq 1 ]; then
                NEWLY_BROKEN+=("$curr_fail")
            fi
        done

        if [ ${#NEWLY_FIXED[@]} -gt 0 ]; then
            echo -e "${GREEN}Newly Fixed Tests:${NC}"
            for test in "${NEWLY_FIXED[@]}"; do
                echo -e "  ${GREEN}✓${NC} Test $test"
            done
            echo ""
        fi

        if [ ${#NEWLY_BROKEN[@]} -gt 0 ]; then
            echo -e "${RED}Newly Broken Tests:${NC}"
            for test in "${NEWLY_BROKEN[@]}"; do
                echo -e "  ${RED}✗${NC} Test $test"
            done
            echo ""
        fi

        if [ ${#NEWLY_FIXED[@]} -eq 0 ] && [ ${#NEWLY_BROKEN[@]} -eq 0 ]; then
            echo -e "${BLUE}No changes in test results${NC}"
            echo ""
        fi
    fi

    # Display failed tests if any
    if [ $FAILED_TESTS -gt 0 ]; then
        echo -e "${RED}========================================${NC}"
        echo -e "${RED}  Currently Failed Tests${NC}"
        echo -e "${RED}========================================${NC}"
        echo ""
        for failure in "${FAILED_LIST[@]}"; do
            # Check if bonus
            failnum=$(echo "$failure" | sed 's/c\([0-9]*\).*/\1/')
            if [ "$failnum" -ge 27 ] 2>/dev/null; then
                echo -e "  ${RED}✗${NC} Test $failure ${CYAN}(BONUS)${NC}"
            else
                echo -e "  ${RED}✗${NC} Test $failure"
            fi

            # Show where to find the diff
            if [ -f "$RESULTS_DIR/diff_${failure}.txt" ]; then
                echo -e "      Diff saved: ${RESULTS_DIR}/diff_${failure}.txt"
            fi
        done
        echo ""
        echo -e "${YELLOW}Tip: Check the diff files in ${RESULTS_DIR}/ for details${NC}"
        echo ""
    else
        echo -e "${GREEN}========================================${NC}"
        echo -e "${GREEN}  All tests passed! 🎉${NC}"
        echo -e "${GREEN}========================================${NC}"
        echo ""
    fi

    # Store current results for next comparison
    PREV_PASSED_TESTS=$PASSED_TESTS
    PREV_FAILED_TESTS=$FAILED_TESTS
    PREV_PERCENTAGE=$PERCENTAGE
    PREV_FAILED_LIST=("${FAILED_LIST[@]}")

    return 0
}

# Function to show specific test details
show_test_details() {
    local testname=$1

    echo -e "${CYAN}========================================${NC}"
    echo -e "${CYAN}  Test Details: ${testname}${NC}"
    echo -e "${CYAN}========================================${NC}"
    echo ""

    local testfile="$TESTCASES_DIR/${testname}.mc"
    local solutionfile="$SOLUTIONS_DIR/${testname}.mc.sol"
    local outputfile="$TEMP_DIR/output_${testname}.txt"
    local difffile="$RESULTS_DIR/diff_${testname}.txt"

    if [ -f "$testfile" ]; then
        echo -e "${YELLOW}Test Input (${testname}.mc):${NC}"
        cat "$testfile"
        echo ""
    else
        echo -e "${RED}Test file not found: $testfile${NC}"
        echo ""
    fi

    if [ -f "$solutionfile" ]; then
        echo -e "${YELLOW}Expected Output:${NC}"
        cat "$solutionfile"
        echo ""
    else
        echo -e "${RED}Solution file not found${NC}"
        echo ""
    fi

    if [ -f "$outputfile" ]; then
        echo -e "${YELLOW}Actual Output:${NC}"
        cat "$outputfile"
        echo ""
    else
        echo -e "${RED}Output file not found (test may not have run yet)${NC}"
        echo ""
    fi

    if [ -f "$difffile" ]; then
        echo -e "${YELLOW}Differences:${NC}"
        cat "$difffile"
        echo ""
    else
        echo -e "${GREEN}No differences found (test passed or not compared yet)${NC}"
        echo ""
    fi
}

# Function to list available tests
list_tests() {
    echo -e "${CYAN}========================================${NC}"
    echo -e "${CYAN}  Available Tests${NC}"
    echo -e "${CYAN}========================================${NC}"
    echo ""
    
    echo -e "${MAGENTA}Core Tests:${NC}"
    for testfile in "$TESTCASES_DIR"/*.mc; do
        if [ -f "$testfile" ]; then
            filename=$(basename "$testfile")
            testname="${filename%.mc}"
            testnum=$(echo "$testname" | sed 's/c\([0-9]*\).*/\1/')
            if [ "$testnum" -lt 27 ] 2>/dev/null; then
                echo -e "  ${testname}"
            fi
        fi
    done
    echo ""
    
    echo -e "${CYAN}Bonus Tests:${NC}"
    for testfile in "$TESTCASES_DIR"/*.mc; do
        if [ -f "$testfile" ]; then
            filename=$(basename "$testfile")
            testname="${filename%.mc}"
            testnum=$(echo "$testname" | sed 's/c\([0-9]*\).*/\1/')
            if [ "$testnum" -ge 27 ] 2>/dev/null; then
                echo -e "  ${testname}"
            fi
        fi
    done
    echo ""
}

# Main loop
while true; do
    run_tests

    # Prompt user
    echo ""
    echo -e "${MAGENTA}========================================${NC}"
    echo -e "${YELLOW}Options:${NC}"
    echo -e "  ${GREEN}r${NC} - Run tests again"
    echo -e "  ${GREEN}d [name]${NC} - Show details for test (e.g., 'd c5' or 'd c15.1')"
    echo -e "  ${GREEN}l${NC} - List all available tests"
    echo -e "  ${GREEN}q${NC} - Quit"
    echo ""
    echo -n -e "${YELLOW}Choose an option: ${NC}"
    read -r response

    # Convert to lowercase
    response=$(echo "$response" | tr '[:upper:]' '[:lower:]')

    if [ "$response" == "q" ] || [ "$response" == "quit" ]; then
        echo -e "${BLUE}Exiting test suite. Goodbye!${NC}"
        break
    elif [ "$response" == "r" ] || [ "$response" == "run" ] || [ "$response" == "y" ] || [ "$response" == "yes" ]; then
        echo ""
        echo ""
        continue
    elif [ "$response" == "l" ] || [ "$response" == "list" ]; then
        list_tests
        echo ""
        echo -n -e "${YELLOW}Press Enter to continue...${NC}"
        read -r
        echo ""
    elif [[ "$response" == d* ]]; then
        # Extract test name
        testname=$(echo "$response" | sed 's/^d *//')
        # Remove 'c' prefix if user included it
        if [[ ! "$testname" =~ ^c ]]; then
            testname="c${testname}"
        fi
        if [ -f "$TESTCASES_DIR/${testname}.mc" ]; then
            show_test_details "$testname"
            echo ""
            echo -n -e "${YELLOW}Press Enter to continue...${NC}"
            read -r
            echo ""
        else
            echo -e "${RED}Test not found: ${testname}${NC}"
            echo -e "${YELLOW}Use 'l' to list available tests${NC}"
            sleep 2
        fi
    else
        echo ""
        echo ""
        continue
    fi
done

# Clean up temp directories
echo -e "${YELLOW}Cleaning up temporary files...${NC}"
rm -rf "$TEMP_DIR"
rm -rf "$RESULTS_DIR"

echo -e "${GREEN}Done!${NC}"
exit 0
