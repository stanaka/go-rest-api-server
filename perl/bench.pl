#!/usr/bin/env perl
use strict;
use warnings;

use Parallel::ForkManager;
use DBI;
use Cache::Memcached::Fast;
use Getopt::Long;
use Time::HiRes qw(time);
use Furl;

my $total = 10000;
my $concurrency = 10;
my $method = "mysql";
my $host = "localhost";
my $output = "normal";

GetOptions ("n=i" => \$total,
	    "c=i"   => \$concurrency,
	    "h=s"   => \$host,
	    "t=s"  => \$method,
            "o=s"  => \$output)
  or die("Error in command line arguments\n");

my $memd;
my $dbh;
my $id = 0;

sub fetch_mysql {
    $id++;
    warn $id if($id > 10000);
    $id = 0 if($id > 10000);
    $dbh ||= DBI->connect('DBI:mysql:test:' . $host . ':3306', 'test', 'test');
    my $sth = $dbh->prepare("select name,mail from user where id = ?");
    $sth->execute($id);
    my $row = $sth->fetchrow_hashref;
    $sth->finish;
    return $row;
}

sub fetch_memcached {
    $id++;
    warn $id if($id > 10000);
    $id = 0 if($id > 10000);
    $memd ||= new Cache::Memcached::Fast({
	servers => [ { address => $host . ':11211' } ],
    });
    my @dat = split(/\|/, $memd->get($id), 2);
    return {name => $dat[0], mail => $dat[1]};
}

my $furl;
sub fetch_http {
    $id++;
    warn $id if($id > 10000);
    $id = 0 if($id > 10000);
    $furl ||= Furl->new(
        agent   => 'MyGreatUA/2.0',
        timeout => 10,
    );

    my $res = $furl->get($host);
    warn $res->status_line unless $res->is_success;
    return $res->content;
}

my $pm= Parallel::ForkManager->new($concurrency);
my $t0 = time();
for(my $p = 0; $p < $concurrency; $p++){
  $pm->start and next;
  for(my $i = 0; $i < $total / $concurrency; $i++){
    if($method eq 'mysql'){
      fetch_mysql();
    } elsif($method eq 'http'){
      fetch_http();
    } else {
      fetch_memcached();
    }
  }
  $pm->finish;
};

$pm->wait_all_children;
my $t1 = time();
my $td = $t1-$t0;

if($output eq 'csv'){
    printf "target,concurrecy,time taken,num of requests,requests per second,time per request\n";
    printf "%s,%d,%.3f,%d,%.2f,%.3f\n", $host, $concurrency, $td, $total, $total / $td, $td * 1000 / $total;
} else {
    printf "Target: %s\n", $host;
    printf "Concurrency Level: %d\n", $concurrency;
    printf "Time taken for tests:   %.3f seconds\n", $td;
    printf "Num of requests: %d\n", $total;
    printf "Requests per second: %.2f [#/sec] (average)\n", $total / $td;
    printf "Time per request: %.3f [ms] (average)\n", $td * 1000 / $total;
}


