use inc::Module::Install;

if ( $Module::Install::AUTHOR ) {
    system('pod2text lib/Math/Round/Fair.pm > README');
}

all_from 'lib/Math/Round/Fair.pm';

# resources
homepage   'http://github.com/semifor/math-round-fair';
bugtracker 'http://rt.cpan.org/Public/Dist/Display.html?Name=Math-Round-Fair';

requires 'Carp';
requires 'Exporter';
test_requires 'Test::More';
test_requires 'List::Util';

auto_set_repository;
auto_manifest;
WriteAll;
