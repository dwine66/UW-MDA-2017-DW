# UW Data Science Winter 2017
# Dave Wine 8430191
# Term Project - Refugees, Terrorism, and Airstrikes

# Import packages
require(ggplot2)
require(car)
require(plyr)
require(dplyr)

# Perspectives
# 1. You are a US customs official with a long line of refugees.  You need to decide whether each is a potential
# based on their sex, country of origin, and religion.
# 2. You are a terrorist mastermind trying to recruit people who will not arouse US customs supervision.
# 3. You are the President of the United States trying to choose an entry policy.
# 4. You are a citizen of the United States that is voting for a President that is claiming extreme danger
# from foreign terrorists posing as refugees.
# 5. You are a citizen of one of those countries, which is currently subject to airstrikes by the United
# States.  How safe are you compared to the US citizen?
# 6. You are a policy wonk in the US government, trying to decide whether to add additional screening to 
# a refugee admission process.