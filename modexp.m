%{
    Problem 7 (i): modexp function
    Returns x ^ y mod n for x, y, and n > 1.
%}
function result = modexp (x, y, n)
    %anything raised to 0th power = 1 so return 1
    if (y == 0)
        result = 1;
        return;
    end
 
    %recurse
    z = modexp(x, floor(y/2), n);
 
    %if even square the result
    if (mod(y, 2) == 0)
        result = mod(z*z, n);
        return;
    else
        %odd so square the result & multiply by itself
        result = mod(x*z*z, n);
        return;
    end
end
