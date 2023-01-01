library(tidyverse)
########################################
# Give Retention time values (seconds) 
# of the alkane series 
########################################
alkanes = data.frame(
RT = 
c(
c8 <- 523.5, # n-octane
c9 <- 717.6, # n-nonane
c10 <- 906.8, # n-decane
c11 <- 1085.8, # n-undecane
c12 <- 1254.4, # n-docecane
c13 <- 1411.7, # n-tridecane
c14 <- 1535.6, # n-tetradecane
c15 <- 1633.1, # n-pentadecan
c16 <- 1715.8, #n-hexadecanee
c17 <- 1788.8, #n-heptadecane
c18 <- 1855,  # n-octadecane
c19 <- 1916.8 # n-nonadecane
),
carbons = c(8:19)
)


# Plot retention time progression of the alkane series
alkanes %>% 
  slice(c(1:4, 8:19)) %>% 
ggplot(aes(x = carbons,  y = RT))+
  geom_point()+
  geom_line()+
  ylab("retention_time")+
  scale_x_continuous(breaks = seq(0, 40, by = 1))


###########################################################
# Set the N-1 / N+1 points from your compound of interest #
###########################################################

# Measured retention time of your compound of interest
# This is now done using the KI_notebook.Rmd file! 
# my.rt = 1017


# Logical statement to determine the retention time interval of your compound
# And set the variables needed to calculate your KI:
#
# RT1 = Retention time of the first alkane before "my.rt" 
# RT2 = Retention time of the first alkane after "my.rt" 
# N = The number of carbons of the alkane that comes after "my.rt"

KI.variables = 
  if ((my.rt > c8) & (my.rt < c9) ) {
  list("RT1" = c8,
  "RT2"= c9,
  "N" = 9
  )
} else{
    if ((my.rt > c9) & (my.rt < c10) ) {
      list("RT1" = c9,
      "RT2"= c10,
      "N" = 10
      )
    } else{
        if ((my.rt > c10) & (my.rt < c11) ) {
          list("RT1" = c10,
          "RT2"= c11,
          "N" = 11
          )
        }  else{
          if ((my.rt > c11) & (my.rt < c12) ) {
           list("RT1" = c11,
            "RT2"= c12,
            "N" = 12
           )
          }  else{
            if ((my.rt > c12) & (my.rt < c13) ) {
              list("RT1" = c12,
              "RT2"= c13,
              "N" = 13
              )
            } else{
              if ((my.rt > c13) & (my.rt < c14) ) {
               list("RT1" = c13,
                "RT2"= c14,
                "N" = 14
               )
              } else{
                if ((my.rt > c14) & (my.rt < c15) ) {
                  list("RT1" = c14,
                  "RT2"= c15,
                  "N" = 15
                  )
                } else{
                  if ((my.rt > c15) & (my.rt < c16) ) {
                    list("RT1" = c15,
                    "RT2"= c16,
                    "N" = 16
                    )
                  }  else{
                    if ((my.rt > c16) & (my.rt < c17) ) {
                      list("RT1" = c16,
                      "RT2"= c17,
                      "N" = 17
                      )
                    }  else{
                      if ((my.rt > c17) & (my.rt < c18) ) {
                        list("RT1" = c17,
                        "RT2"= c18,
                        "N" = 18
                        )
                      }  else{
                        if ((my.rt > c18) & (my.rt < c19) ) {
                          list("RT1" = c18,
                          "RT2"= c19,
                          "N" = 19
                          )
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
  }


#############################################
# Calculate KI using van der Dool's formula #
#############################################
KI = round((100 * KI.variables$N)+100*((my.rt - KI.variables$RT1)/(KI.variables$RT2 - KI.variables$RT1)))
