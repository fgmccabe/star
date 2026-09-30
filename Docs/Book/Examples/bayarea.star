book.examples.map.bayarea{
  import star.

  import book.examples.map.city.

  -- Top 20 cities in the SF bay area by population.

  bayarea = [
    city{name="San Jose", county="Santa Clara", pop=984000},
    city{name="San Francisco", county="San Francisco", pop=812000},
    city{name="Oakland", county="Alameda", pop=440000},
    city{name="Fremont", county="Alameda", pop=228000},
    city{name="Santa Rosa", county="Sonoma", pop=177000},
    city{name="Hayward", county="Alameda", pop=162000},
    city{name="Sunnyvale", county="Santa Clara", pop=155000},
    city{name="Santa Clara", county="Santa Clara", pop=133000},
    city{name="Concord", county="Contra Costa", pop=123000},
    city{name="Vallejo", county="Solano", pop=122000},
    city{name="Berkeley", county="Alameda", pop=121000},
    city{name="Fairfield", county="Solano", pop=119000},
    city{name="Antioch", county="Contra Costa", pop=116000},
    city{name="Richmond", county="Contra Costa", pop=115000},
    city{name="Daly City", county="San Mateo", pop=104000},
    city{name="San Mateo", county="San Mateo", pop=103000},
    city{name="Vacaville", county="Solano", pop=102000},
    city{name="San Ramon", county="Contra Costa", pop=86000},
    city{name="Mountain View", county="Santa Clara", pop=85000},
    city{name="San Leandro", county="Alameda", pop=84,000}
  ].

  distances = [
    .neighbor("San Jose", "Santa Clara",5),
    .neighbor("Santa Clara","Sunnyvale",6),
    .neighbor("Sunnyvale", "Mountain View",4),
    .neighbor("San Francisco", "Daly City",8),
    .neighbor("Daly City", "San Mateo"16),
    .neighbor("San Francisco", "Oakland", 12),
    .neighbor("Oakland", "Berkeley", 5),
    .neighbor("Oakland", "San Leandro", 9),
    .neighbor("San Leandro", "Hayward", 5),
    .neighbor("Hayward", "Fremont", 11),
    .neighbor("Concord", "Antioch", 18),
    .neighbor("Vallejo", "Fairfield", 18),
    .neighbor("Fairfield", "Vacaville", 10),
    .neighbor("Santa Rosa", "Vallejo", 40),
    .neighbor("Berkeley", "Richmond", 9),
    .neighbor("Concord", "San Ramon", 16),
    .neighbor("San Ramon", "Hayward", 14),
    .neighbor("Concord", "Antioch", 18),
    .neighbor("San Mateo", "Fremont", 20),
    .neighbor("Mountain View", "Fremont", 19),
    .neighbor("Fremont", "San Jose", 17)
    ]
}
