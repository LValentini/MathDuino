<?php
        
    $delta = rand(99.5,100.5) / 100.0;
    $deltapressure = rand(99.99,100.01) / 100.0;
$dc = <<<EOD
<?xml version="1.0" encoding="UTF-8"?>
<datacollector>
<dcid>94:0c:6d:b1:8a:65</dcid>
<geoloc>
<ap>
<channel>02</channel>
<rssi>-42</rssi>
<mac>00:18:84:a5:65:35</mac>
</ap>
<ap>
<channel>06</channel>
<rssi>-75</rssi>
<mac>00:1d:8b:5c:af:60</mac>
</ap>
<ap>
<channel>06</channel>
<rssi>-51</rssi>
<mac>a0:21:b7:84:8c:1c</mac>
</ap>
<ap>
<channel>09</channel>                                                                                                                                                                                                                                                         
<rssi>-80</rssi>                                                                                                                                                                                                                                                              
<mac>00:1d:6a:d7:e2:4f</mac>                                                                                                                                                                                                                                                  
</ap>                                                                                                                                                                                                                                                                         
</geoloc>
<sensors>
<sensor><type>temp</type><value>%f</value></sensor>
<sensor><type>light</type><value>%f</value></sensor>
<sensor><type>humidity</type><value>NA</value></sensor>
<sensor><type>pressure</type><value>%f</value></sensor>
</sensors>
</datacollector>
EOD;
            printf($dc, 27.0 * $delta, 160.0 * $delta, 1016 * $deltapressure );
?>
