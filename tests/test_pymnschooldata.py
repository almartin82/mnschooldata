"""
Tests for pymnschooldata Python wrapper.

Minimal smoke tests - the actual data logic is tested by R testthat.
These just verify the Python wrapper imports and exposes expected functions.
"""

import pytest


def test_import_package():
    """Package imports successfully."""
    import pymnschooldata
    assert pymnschooldata is not None


def test_has_fetch_enr():
    """fetch_enr function is available."""
    import pymnschooldata
    assert hasattr(pymnschooldata, 'fetch_enr')
    assert callable(pymnschooldata.fetch_enr)


def test_has_get_available_years():
    """get_available_years function is available."""
    import pymnschooldata
    assert hasattr(pymnschooldata, 'get_available_years')
    assert callable(pymnschooldata.get_available_years)


def test_has_version():
    """Package has a version string."""
    import pymnschooldata
    assert hasattr(pymnschooldata, '__version__')
    assert isinstance(pymnschooldata.__version__, str)
