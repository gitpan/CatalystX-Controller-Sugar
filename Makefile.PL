use inc::Module::Install;
name q(CatalystX-Controller-Sugar);
all_from q(lib/CatalystX/Controller/Sugar.pm);
requires q(Catalyst) => 5.80021;
requires q(Data::Dumper) => 2.0;
requires q(namespace::autoclean) => 0.09;
test_requires q(Test::More) => 0.9;
auto_insall;
WriteAll;
