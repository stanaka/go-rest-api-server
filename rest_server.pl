# plackup -s Starman -E production rest_server.pl
use Plack::Builder;
use DBI;
use Data::MessagePack;
use JSON::XS;
use Cache::Memcached::Fast;

my $mp = Data::MessagePack->new();
my $dbh = DBI->connect('DBI:mysql:test:localhost:3307', 'root', '');
my $memd = new Cache::Memcached::Fast({
    servers => [ { address => 'localhost:11211' } ],
});
my $id = 0;

sub fetch_mysql {
    $id++;
    $id = 0 if($id > 10000);
    my $sth = $dbh->prepare("select name,mail from user where id = ?");
    $sth->execute($id);
    my $row = $sth->fetchrow_hashref;
    $sth->finish;
    return $row;
}

sub fetch_memcached {
    $id++;
    @dat = split(/\|/, $memd->get($id), 2);
    return {name => $dat[0], mail => $dat[1]};
}

builder {
    # enable 'AxsLog', combined => 1, logger => sub {};
    # enable 'AccessLog', logger => sub { };
    sub{
        my $env = shift;
        #use Data::Dumper;
        #warn Data::Dumper::Dumper $env;

        if($env->{REQUEST_URI} eq '/'){
            return [200,['Content-Type','application/x-msgpack'],[$mp->pack(fetch_mysql())]];
        } elsif($env->{REQUEST_URI} eq '/json'){
            return [200,['Content-Type','application/json'],[encode_json(fetch_mysql())]];
        } elsif($env->{REQUEST_URI} eq '/mem'){
            return [200,['Content-Type','application/x-msgpack'],[$mp->pack(fetch_memcached())]];
        } else {
            return [200,['Content-Type','application/json'],[encode_json(fetch_memcached())]];
        }
    }
};

