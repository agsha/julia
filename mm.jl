using LibPQ, Tables, HTTP, CSV, Random, StatsBase, FreqTables, SQLite, Base, Dates, JuliaDB, Statistics, 
StructArrays, Distributions, NPFinancial, Random, DataStructures, PlotlyJS;
using Base: show_supertypes;
import Base: iterate;
import Base.Threads.@spawn


function p2(p1::Number, r::Number, t::Number)
    return Float64(p1)*((1+Float64(r))^Float64(t))
end

function t(p1::Number, p2::Number, r::Number)
    return log(Float64(p2)/Float64(p1)) / log(1+Float64(r))
end

function p1(p2::Number, r::Number, t::Number)
    return Float64(p2)/((1+Float64(r))^Float64(t))
end

function r(p1::Number, p2::Number, t::Number)
    power = log(Float64(p2)/Float64(p1))/Float64(t)
    return exp(power) - 1
end

function emi(p1::Number, r::Number, t::Number)
    x::Float64 = (1.0 + Float64(r))^Float64(t)
    return Float64(p1) * Float64(r) * x / (x - 1)
end
function splt(s, delim=r"\s+")
    ar = split(s, delim)
    ar = strip.(ar)
    ar = filter(s -> s != Missing && length(s)>0, ar)
    return ar
end
import Base: iterate
import Base: push!
mutable struct FastListNode{T}
    prev::Int
    next::Int
    data::T
    function FastListNode{T}() where T
        node = new{T}()
        return node
    end
end
mutable struct FastList{T}
    len::Int
    sz::Int # the next free node in the list
    list::Vector{FastListNode{T}}
    function FastList{T}(l::Int) where T
        fl = new{T}()
        fl.list = Vector{FastListNode{T}}(undef, l)
        for i in 1:l
            fl.list[i] = FastListNode{T}()
        end
        fl.len = 0
        fl.list[1].prev = fl.list[1].next = 1
        fl.sz = 2 # the first node is the root
        return fl
    end
end

function push!(fl::FastList{T}, data::T) where T
    list::Vector{FastListNode{T}} = fl.list
    node = list[fl.sz]
    node.prev = list[1].prev
    node.next = 1
    node.data = data

    list[list[1].prev].next = fl.sz
    list[1].prev = fl.sz
    fl.len += 1
    fl.sz += 1
end

function del(fl::FastList{T}, node::FastListNode{T}) where T
    fl.len==0 && throw(ArgumentError("List must be non-empty"))
    list::Vector{FastListNode{T}} = fl.list
    list[node.prev].next = node.next
    list[node.next].prev = node.prev
    fl.len -= 1
end

function reset!(fl::FastList{T}) where T
    fl.len = 0
    fl.list[1].prev = fl.list[1].next = 1
    fl.sz = 2 # the first node is the root
end

function Base.iterate(fl::FastList{T}) where T
    if fl.len == 0
        return nothing
    end
    list::Vector{FastListNode{T}} = fl.list
    return (list[list[1].next], list[list[1].next].next)
end

function iterate(fl::FastList{T}, n::Int) where T
    if n==1
        return nothing
    end
    return (fl.list[n], fl.list[n].next)

end

function pr(fl::FastList)
    for node in fl
        println(node.data)
    end
end
conn = LibPQ.Connection("dbname=sharath")
sqlite = SQLite.DB()
struct Point
    dates::Array{DateTime}
    prices::Array{Float64}
    intDates::Vector{Int}
end

function getSeries(symbol::String)
    sql = """select date, adjclose from nse_raw nr where symbol = '"""*symbol*"""' and adjclose is not null order by 1 """
    result = execute(conn, sql)
    println(show_supertypes(typeof(result)))
    return result |> table
end
#v2 a linear time algorithm for calculating sip returns. returns an array of returns.
# each element corresponds to a period of sipDurationDays
function sipv2(sipFrequencyDays::Int, sipDurationDays::Int, series)
    series2 = Array{Float64}(undef, 0)
    stocksNow = 0.0
    for i in 1:sipFrequencyDays:length(series)
        append!(series2, series[i])
    end
    n = size(series2)[1]
    rates = Vector{Float64}(undef, 0)
    stocks::Float64 = 0.0
    sipDurationDays = min(n, convert(Int64, floor(sipDurationDays/sipFrequencyDays)))

    
    for i in 1:sipDurationDays
        stocks+=1/series2[i]
    end
    append!(rates, r(sipDurationDays, series[sipDurationDays]*stocks, sipDurationDays/365.0))

    for i in 2:n-sipDurationDays+1
        stocks -= 1/series[i-1]
        stocks += 1/series[i+sipDurationDays-1]
        rate = r(sipDurationDays, series[i+sipDurationDays-1]*stocks, sipDurationDays/365.0)
        append!(rates, r(sipDurationDays, series[i+sipDurationDays-1]*stocks, sipDurationDays/365.0))
    end
    return rates
end
# println(typeof(returns[1, :]))
function gogo(symbol)
    years = 1:7
    returns = Array{Float64}(undef, 101, length(years))

    for year in years
        returns[:, year] = nquantile(sipv2(1, (year)*365, d[symbol].prices), 100).*100
    end
    return plot(collect(1:101), returns, label = permutedims(["year"*string(x) for x in years])) 
end

function constructDict()::Dict{String, Point}
	start = DateTime(1900,1,1)
    data = columntable(execute(conn, """ select symbol, date, adjclose from nse_raw where date is not null and adjclose is not null order by 1, 2"""))
    symbols = copy(data[1])
    dates = copy(data[2])
    adjclose = copy(data[3])
    d = Dict{String, Point}()
    for i in 1:length(symbols)
        symbol = symbols[i]
        date = dates[i]
        price = adjclose[i]
        if price < 1
            continue
        end
        if !haskey(d, symbol)
            d[symbol] = Point(Array{DateTime}(undef, 0), Array{Float64}(undef, 0), Vector{Int}(undef, 0))
        end
        point = d[symbol]
        push!(point.dates, dates[i])
        push!(point.prices, max(0, adjclose[i]))
        push!(point.intDates, Dates.value(convert(Dates.Day, dates[i] - start)))
    end
    return d
end

#returns the probablity of a "good" return
# the random experiment is: select an arbitrary start date and do sip for sipDuration
# "good" is defined as yes if the experiment gave an interest more than targetRate
function prob(d::Dict{String, Point}, company::String, sipFrequencyDays::Int, sipDurationDays::Int, targetRate::Float64=0.1)
    series = d[company].prices
    returns = sipv2(sipFrequencyDays::Int, sipDurationDays::Int, series)
    sorted = sort(returns)
    bottom = length(returns)
    top = min(bottom, first(searchsorted(sorted, targetRate)))
    return (bottom-top)/bottom
end

function doplot(d::Dict{String, Point})
    k = collect(keys(d))
    tuples = [(i, prob(d, i, 1, 3*365, 0.1)) for i in k]
    filter(x->x[2]>0.7,sort!( tuples, by=last))
    
    return plot(getindex.(tuples,1), getindex.(tuples, 2))
end
function doplot(x, y)
    plot(x, y; hover=fmt.(x).*"   ".*fmt.(y), size=(1200, 400,))
end

function stock(symbol::String)
    display(plot(d[symbol].dates, d[symbol].prices, title="stock prices"; hover=fmt.(d[symbol].dates).*"   ".*string.(d[symbol].prices), size=(1700, 400,)))
#     display(gogo(symbol))
end

function fti(x::Number)
    return convert(Int64, round(Float64(x)))
end
function fmt(d::DateTime)
    Dates.format(d, "yyyy-mm-dd")
end
function fmt(d::Any)
    string(d)
end
function weiner(days::Int, μ::Float64, σ::Float64, s::Vector{Float64}, 
        ϕ::Vector{Float64}, dist::Normal{Float64})
    s[1] = 100
    a::Float64 = μ/365.0
    b::Float64 = σ/sqrt(365.0)
    rand!(dist, ϕ)
    for i::Int in 2:days
        s[i] = s[i-1]*(1 + a + ϕ[i]*b)
    end  
    return s
end

function downscale(Y::Vector{Float64}, points::Int)
    days::Int = length(Y)
    step::Float64 = 1
    if days > points
        step = days / points
    end
    println(step)
    XX = Vector{Float64}(undef, 0)
    YY = Vector{Float64}(undef, 0)
    for i in 1:step:days
        push!(XX, Int(i))
        push!(YY, Y[min(days, Int(round(i)))])
    end
    return XX, YY
end
function simulate(d::Dict{String, Point})
    p = d["^NSEI"];
    
    dates, prices = p.intDates, p.prices
    wallets::Vector{Float64} = Vector{Float64}(undef, 1)

    ll = FastList{Int}(10000);
    investDays::Int = 365*2; redeemDays::Int = 365; days = investDays + redeemDays
    for start in 1:length(dates) - days - 10
        ddates = dates[start:end];
        pprices = prices[start:end];
        wallet = oneRun(0.15, investDays, redeemDays, ll,
        ddates, pprices);
        push!(wallets, wallet);
    end
end
