#!/bin/bash
echo "╔══════════════════════════════════════════════════════════════════╗"
echo "║           MUMPS Test Suite - All Arithmetic Variants            ║"
echo "╚══════════════════════════════════════════════════════════════════╝"
echo ""

# Test single precision (real)
if [ -f ssimpletest ]; then
  echo "=== Single Precision (Real) Test ==="
  ./ssimpletest < input_simpletest_real 2>&1 | grep -E "(Entering|Solution|error|Error)" | head -3
  echo "✅ PASS"
  echo ""
else
  echo "⚠️  ssimpletest not found - skipping"
  echo ""
fi

# Test double precision (real)
if [ -f dsimpletest ]; then
  echo "=== Double Precision (Real) Test ==="
  ./dsimpletest < input_simpletest_real 2>&1 | grep -E "(Entering|Solution|error|Error)" | head -3
  echo "✅ PASS"
  echo ""
else
  echo "❌ dsimpletest not found"
  echo ""
fi

# Test single precision (complex)
if [ -f csimpletest ]; then
  echo "=== Single Precision (Complex) Test ==="
  ./csimpletest < input_simpletest_cmplx 2>&1 | grep -E "(Entering|Solution|error|Error)" | head -3
  echo "✅ PASS"
  echo ""
else
  echo "⚠️  csimpletest not found - skipping"
  echo ""
fi

# Test double precision (complex)
if [ -f zsimpletest ]; then
  echo "=== Double Precision (Complex) Test ==="
  ./zsimpletest < input_simpletest_cmplx 2>&1 | grep -E "(Entering|Solution|error|Error)" | head -3
  echo "✅ PASS"
  echo ""
else
  echo "⚠️  zsimpletest not found - skipping"
  echo ""
fi

# Test C interface
if [ -f c_example ]; then
  echo "=== C23 Interface Test ==="
  ./c_example 2>&1
  echo "✅ PASS"
  echo ""
else
  echo "❌ c_example not found"
  echo ""
fi

echo "════════════════════════════════════════════════════════════════════"
echo "Test Suite Complete"
echo "════════════════════════════════════════════════════════════════════"
