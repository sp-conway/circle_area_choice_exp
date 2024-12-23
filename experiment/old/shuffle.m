function s = shuffle(v)
    s = v(randperm(length(v)));
end 

shuffle([1 2 3])
