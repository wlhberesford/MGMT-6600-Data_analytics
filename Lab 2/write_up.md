# Lab 2
Liam Beresford

## Linear Models
### Price ~ PROPERTYSQFT
> Modeled a simple relationship between price of the property and its square footage. This model was found to have a positive relationship. This has a weak relationship (r <sup>2</sup>=0.19) due to its basic nature but still was able to capture the general trend. 

### Price ~ BEDS + BATHS
> This model shows the effect of the number of bedrooms and bathrooms on the price of the property. Despite being slightly more complex than the first model this only had an r <sup>2</sup> of 0.14. This shows that the square footage is a better predictor of the price of a property. 

### Price ~ PROPERTYSQFT + BATH + BEDS + (BATH * BEDS)

> This being the most complex of the three models include all studied parameters and an interaction term between bedrooms and bathrooms to capture the relationship for situations like appartments without private bathrooms. This was the strongest of the three models with a r <sup>2 </sup> of 0.26. 

## Review
Given only working with these three parameters, the last model proved to be the strongest. It had the highest correlation coefficent of the 3 while having all coefficients being signifigant. For the scope of this assignment this is about as good of a model that could be made. Future work to improve the model's predicitive power would require the inclusion of more paramteres (location, building type, etc).
