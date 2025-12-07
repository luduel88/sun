library(rsconnect)

rsconnect::setAccountInfo(name='luduel',
                          token='A56A7886C2FB98AD1D68AAB424E00BF5',
                          secret='qGMOhBTAx/3J2KZk4qAbG8bFht4CbzirXGDXB9yF')

rsconnect::deployApp("~/sun/sun_app")

rsconnect::showLogs(appName = "sun_app")
