#
# spec file for package erlang-jsonnet
#

%define app_name jsonnet
Name:           erlang-%{app_name}
Version:        0.6.0
Release:        0
%define app_ver %(echo "%{version}" | cut -d "+" -f1)
Summary:        Jsonnet for Erlang library
License:        Apache License, Version 2.0
Group:          Development/Libraries/Other
Url:            https://github.com/ray2501/erlang-jsonnet
Source:         %{name}-%{version}.tar.gz
BuildRequires:  make
BuildRequires:  gcc-c++
BuildRequires:  libstdc++-devel
BuildRequires:  erlang
Requires:       erlang
BuildRoot:      %{_tmppath}/%{name}-%{version}-build

%description
It is a Jsonnet for Erlang library.

%prep
%setup -q -n %{name}-%{version}

%build
make

%install
for dir in ebin priv ; do
	mkdir -p %{buildroot}%{erlang_libdir}/%{app_name}-%{app_ver}/${dir}
	cp -r ${dir}/* %{buildroot}%{erlang_libdir}/%{app_name}-%{app_ver}/${dir}/
done

%files
%defattr(-,root,root)
%doc README.md LICENSE
%dir %{erlang_libdir}/%{app_name}-%{app_ver}
%dir %{erlang_libdir}/%{app_name}-%{app_ver}/ebin
%{erlang_libdir}/%{app_name}-%{app_ver}/ebin/*.app
%{erlang_libdir}/%{app_name}-%{app_ver}/ebin/*.beam
%dir %{erlang_libdir}/%{app_name}-%{app_ver}/priv
%{erlang_libdir}/%{app_name}-%{app_ver}/priv/*.so

%changelog

