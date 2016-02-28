#2nd Iteration
train$PersonalField4A  <-  ifelse(train$PersonalField4A  <  0,  0,  1)
train$PersonalField4B  <-  ifelse(train$PersonalField4B  <  0,  0,  1)
train$PersonalField5  <-  ifelse(train$PersonalField5  >  5  
                                 &  train$PersonalField5  <=  7,  1,  0)  
train$PersonalField7  <-  ifelse(train$PersonalField7  == 'N',  1,  0)  
train$PersonalField8  <-  ifelse(train$PersonalField8  == 1,  1,  0)  
train$PersonalField8  <-  ifelse(train$PersonalField8  == 1,  1,  0)  
train$PersonalField10A  <-  ifelse(train$PersonalField10A  >  0  
                                   &  train$PersonalField10A  <=  8,  1,  0)  
train$PersonalField12  <-  ifelse(train$PersonalField12  <  5,  1,  0)  
train$PersonalField13  <-  ifelse(train$PersonalField13  ==  1,  1,  0)  
train$PersonalField14  <-  ifelse(train$PersonalField14  <  10,  1,  0)  
train$PersonalField15  <-  ifelse(train$PersonalField15  <  12,  1,  0)  
train$PersonalField22  <-  ifelse(train$PersonalField22  <  2,  0,  1)  
train$PersonalField23  <-  ifelse(train$PersonalField23  <  1,  0,  1)  
train$PersonalField24  <-  ifelse(train$PersonalField24  <  1,  0,  1)  
train$PersonalField25  <-  ifelse(train$PersonalField25  <  1,  0,  1)  
train$PersonalField26  <-  ifelse(train$PersonalField26  <  1,  0,  1)  
train$PersonalField27  <-  ifelse(train$PersonalField27  <  2,  0,  1)  
train$PersonalField28  <-  ifelse(train$PersonalField28  <  2,  0,  1)  
train$PersonalField29  <-  ifelse(train$PersonalField29  <  4,  0,  1)
train$PersonalField30  <-  ifelse(train$PersonalField30  <  2,  0,  1)
train$PersonalField31  <-  ifelse(train$PersonalField31  <  2,  0,  1)
train$PersonalField32  <-  ifelse(train$PersonalField32  <  2,  0,  1)
train$PersonalField33  <-  ifelse(train$PersonalField33  <  2,  0,  1)
train$PersonalField34  <-  ifelse(train$PersonalField34  <  2,  0,  1)
train$PersonalField35  <-  ifelse(train$PersonalField35  <  1,  0,  1)
train$PersonalField36  <-  ifelse(train$PersonalField36  <  1,  0,  1)
train$PersonalField36  <-  ifelse(train$PersonalField36  <  1,  0,  1)
train$PersonalField38  <-  ifelse(train$PersonalField38  <  1,  0,  1)
train$PersonalField39  <-  ifelse(train$PersonalField39  <  1,  0,  1)
train$PersonalField40  <-  ifelse(train$PersonalField40  <  1,  0,  1)
train$PersonalField41  <-  ifelse(train$PersonalField41  <  1,  0,  1)
train$PersonalField42  <-  ifelse(train$PersonalField42  <  1,  0,  1)
train$PersonalField44  <-  ifelse(train$PersonalField44  <  2,  0,  1)
train$PersonalField45  <-  ifelse(train$PersonalField45  <  2,  0,  1)
train$PersonalField46  <-  ifelse(train$PersonalField46  <  2,  0,  1)
train$PersonalField47  <-  ifelse(train$PersonalField47  <  2,  0,  1)
train$PersonalField48  <-  ifelse(train$PersonalField48  <  4,  0,  1)
train$PersonalField49  <-  ifelse(train$PersonalField49  <  1,  0,  1)
train$PersonalField50  <-  ifelse(train$PersonalField50  <  1,  0,  1)
train$PersonalField51  <-  ifelse(train$PersonalField51  <  1,  0,  1)
train$PersonalField52  <-  ifelse(train$PersonalField52  <  1,  0,  1)
train$PersonalField53  <-  ifelse(train$PersonalField53  <  2,  0,  1)
train$PersonalField54  <-  ifelse(train$PersonalField54  <  1,  0,  1)
train$PersonalField55  <-  ifelse(train$PersonalField55  <  1,  0,  1)
train$PersonalField56  <-  ifelse(train$PersonalField56  <  1,  0,  1)
train$PersonalField57  <-  ifelse(train$PersonalField57  <  1,  0,  1)
train$PersonalField58  <-  ifelse(train$PersonalField58  <  2,  0,  1)
train$PersonalField59  <-  ifelse(train$PersonalField59  <  1,  0,  1)
train$PersonalField60  <-  ifelse(train$PersonalField60  <  1,  0,  1)
train$PersonalField61  <-  ifelse(train$PersonalField61  <  1,  0,  1)
train$PersonalField62  <-  ifelse(train$PersonalField62  <  1,  0,  1)
train$PersonalField64  <-  ifelse(train$PersonalField64  ==  0,  0,  1)
train$PersonalField65  <-  ifelse(train$PersonalField65  ==  0,  0,  1)
train$PersonalField66  <-  ifelse(train$PersonalField66  ==  0,  0,  1)
train$PersonalField67  <-  ifelse(train$PersonalField67  ==  0,  0,  1)
train$PersonalField68  <-  ifelse(train$PersonalField68  <  2,  0,  1)
train$PersonalField69  <-  ifelse(train$PersonalField69  <  1,  0,  1)
train$PersonalField70  <-  ifelse(train$PersonalField70  <  1,  0,  1)
train$PersonalField71  <-  ifelse(train$PersonalField71  <  1,  0,  1)
train$PersonalField72  <-  ifelse(train$PersonalField72  <  1,  0,  1)
train$PersonalField74  <-  ifelse(train$PersonalField74  <  1,  0,  1)
train$PersonalField75  <-  ifelse(train$PersonalField75  <  2,  0,  1)
train$PersonalField76  <-  ifelse(train$PersonalField76  <  2,  0,  1)
train$PersonalField77  <-  ifelse(train$PersonalField77  <  2,  0,  1)
train$PersonalField78  <-  ifelse(train$PersonalField78  <  3,  0,  1)
train$PersonalField79  <-  ifelse(train$PersonalField79  <  1,  0,  1)
train$PersonalField80  <-  ifelse(train$PersonalField80  <  1,  0,  1)
train$PersonalField81  <-  ifelse(train$PersonalField81  <  1,  0,  1)
train$PersonalField82  <-  ifelse(train$PersonalField82  <  1,  0,  1)
train$PersonalField83  <-  ifelse(train$PersonalField83  <  2,  0,  1)

#2nd Iteration
test$PersonalField4A  <-  ifelse(test$PersonalField4A  <  0,  0,  1)
test$PersonalField4B  <-  ifelse(test$PersonalField4B  <  0,  0,  1)
test$PersonalField5  <-  ifelse(test$PersonalField5  >  5  
                                &  test$PersonalField5  <=  7,  1,  0)  
test$PersonalField7  <-  ifelse(test$PersonalField7  == 'N',  1,  0)  
test$PersonalField8  <-  ifelse(test$PersonalField8  == 1,  1,  0)  
test$PersonalField8  <-  ifelse(test$PersonalField8  == 1,  1,  0)  
test$PersonalField10A  <-  ifelse(test$PersonalField10A  >  0  
                                  &  test$PersonalField10A  <=  8,  1,  0)  
test$PersonalField12  <-  ifelse(test$PersonalField12  <  5,  1,  0)  
test$PersonalField13  <-  ifelse(test$PersonalField13  ==  1,  1,  0)  
test$PersonalField14  <-  ifelse(test$PersonalField14  <  10,  1,  0)  
test$PersonalField15  <-  ifelse(test$PersonalField15  <  12,  1,  0)  
test$PersonalField22  <-  ifelse(test$PersonalField22  <  2,  0,  1)  
test$PersonalField23  <-  ifelse(test$PersonalField23  <  1,  0,  1)  
test$PersonalField24  <-  ifelse(test$PersonalField24  <  1,  0,  1)  
test$PersonalField25  <-  ifelse(test$PersonalField25  <  1,  0,  1)  
test$PersonalField26  <-  ifelse(test$PersonalField26  <  1,  0,  1)  
test$PersonalField27  <-  ifelse(test$PersonalField27  <  2,  0,  1)  
test$PersonalField28  <-  ifelse(test$PersonalField28  <  2,  0,  1)  
test$PersonalField29  <-  ifelse(test$PersonalField29  <  4,  0,  1)
test$PersonalField30  <-  ifelse(test$PersonalField30  <  2,  0,  1)
test$PersonalField31  <-  ifelse(test$PersonalField31  <  2,  0,  1)
test$PersonalField32  <-  ifelse(test$PersonalField32  <  2,  0,  1)
test$PersonalField33  <-  ifelse(test$PersonalField33  <  2,  0,  1)
test$PersonalField34  <-  ifelse(test$PersonalField34  <  2,  0,  1)
test$PersonalField35  <-  ifelse(test$PersonalField35  <  1,  0,  1)
test$PersonalField36  <-  ifelse(test$PersonalField36  <  1,  0,  1)
test$PersonalField36  <-  ifelse(test$PersonalField36  <  1,  0,  1)
test$PersonalField38  <-  ifelse(test$PersonalField38  <  1,  0,  1)
test$PersonalField39  <-  ifelse(test$PersonalField39  <  1,  0,  1)
test$PersonalField40  <-  ifelse(test$PersonalField40  <  1,  0,  1)
test$PersonalField41  <-  ifelse(test$PersonalField41  <  1,  0,  1)
test$PersonalField42  <-  ifelse(test$PersonalField42  <  1,  0,  1)
test$PersonalField44  <-  ifelse(test$PersonalField44  <  2,  0,  1)
test$PersonalField45  <-  ifelse(test$PersonalField45  <  2,  0,  1)
test$PersonalField46  <-  ifelse(test$PersonalField46  <  2,  0,  1)
test$PersonalField47  <-  ifelse(test$PersonalField47  <  2,  0,  1)
test$PersonalField48  <-  ifelse(test$PersonalField48  <  4,  0,  1)
test$PersonalField49  <-  ifelse(test$PersonalField49  <  1,  0,  1)
test$PersonalField50  <-  ifelse(test$PersonalField50  <  1,  0,  1)
test$PersonalField51  <-  ifelse(test$PersonalField51  <  1,  0,  1)
test$PersonalField52  <-  ifelse(test$PersonalField52  <  1,  0,  1)
test$PersonalField53  <-  ifelse(test$PersonalField53  <  2,  0,  1)
test$PersonalField54  <-  ifelse(test$PersonalField54  <  1,  0,  1)
test$PersonalField55  <-  ifelse(test$PersonalField55  <  1,  0,  1)
test$PersonalField56  <-  ifelse(test$PersonalField56  <  1,  0,  1)
test$PersonalField57  <-  ifelse(test$PersonalField57  <  1,  0,  1)
test$PersonalField58  <-  ifelse(test$PersonalField58  <  2,  0,  1)
test$PersonalField59  <-  ifelse(test$PersonalField59  <  1,  0,  1)
test$PersonalField60  <-  ifelse(test$PersonalField60  <  1,  0,  1)
test$PersonalField61  <-  ifelse(test$PersonalField61  <  1,  0,  1)
test$PersonalField62  <-  ifelse(test$PersonalField62  <  1,  0,  1)
test$PersonalField64  <-  ifelse(test$PersonalField64  ==  0,  0,  1)
test$PersonalField65  <-  ifelse(test$PersonalField65  ==  0,  0,  1)
test$PersonalField66  <-  ifelse(test$PersonalField66  ==  0,  0,  1)
test$PersonalField67  <-  ifelse(test$PersonalField67  ==  0,  0,  1)
test$PersonalField68  <-  ifelse(test$PersonalField68  <  2,  0,  1)
test$PersonalField69  <-  ifelse(test$PersonalField69  <  1,  0,  1)
test$PersonalField70  <-  ifelse(test$PersonalField70  <  1,  0,  1)
test$PersonalField71  <-  ifelse(test$PersonalField71  <  1,  0,  1)
test$PersonalField72  <-  ifelse(test$PersonalField72  <  1,  0,  1)
test$PersonalField74  <-  ifelse(test$PersonalField74  <  1,  0,  1)
test$PersonalField75  <-  ifelse(test$PersonalField75  <  2,  0,  1)
test$PersonalField76  <-  ifelse(test$PersonalField76  <  2,  0,  1)
test$PersonalField77  <-  ifelse(test$PersonalField77  <  2,  0,  1)
test$PersonalField78  <-  ifelse(test$PersonalField78  <  3,  0,  1)
test$PersonalField79  <-  ifelse(test$PersonalField79  <  1,  0,  1)
test$PersonalField80  <-  ifelse(test$PersonalField80  <  1,  0,  1)
test$PersonalField81  <-  ifelse(test$PersonalField81  <  1,  0,  1)
test$PersonalField82  <-  ifelse(test$PersonalField82  <  1,  0,  1)
test$PersonalField83  <-  ifelse(test$PersonalField83  <  2,  0,  1)




#Modifying other variables
test$SalesField1A  <-  ifelse(test$SalesField1A  <=  5,  1,  0)  
test$SalesField1B  <-  ifelse(test$SalesField1B  <=  13,  1,  0)  
test$SalesField2A  <-  ifelse(test$SalesField2A  >  0  
                              &  test$SalesField2A  <=  7,  1,  0)  
test$SalesField2B  <-  ifelse(test$SalesField2B  >  0  
                              &  test$SalesField2B  <=  18,  1,  0)  
test$SalesField6  <-  ifelse(test$SalesField6  >  2  
                             &  test$SalesField6  <=  21,  1,  0)  
test$SalesField10  <-  ifelse(test$SalesField10  ==  0,  0,  1)
test$SalesField11  <-  ifelse(test$SalesField11  >  0  
                              &  test$SalesField11  <=  9,  1,  0)  
test$SalesField12  <-  ifelse(test$SalesField12  >  0  
                              &  test$SalesField12  <=  11,  1,  0)  
test$SalesField13  <-  ifelse(test$SalesField13  <  4,  1,  0)
test$SalesField14  <-  ifelse(test$SalesField14  <  4,  1,  0)
test$SalesField15  <-  ifelse(test$SalesField15  <  4,  1,  0)
test$Field7  <-  ifelse(test$Field7  >  1  &  test$Field7  <=  16  |  
                          test$Field7  >  23  &  test$Field7  <=  25,  1,  0)  
test$Field8  <-  ifelse(test$Field8  >  .98,  1,  0)
test$Field9  <-  ifelse(test$Field9  >  .00064,  1,  0)
test$Field10  <-  ifelse(test$Field10  <  1000,  1,  0)
test$Field11  <-  ifelse(test$Field11  <  1.2,  1,  0)
test$PropertyField1A  <-  ifelse(test$PropertyField1A  >  10,  1,  0)
test$PropertyField1B  <-  ifelse(test$PropertyField1B  >  14,  1,  0)
test$PropertyField9  <-  ifelse(test$PropertyField9  ==  0,  1,  0)
test$PropertyField10  <-  ifelse(test$PropertyField10  ==  1,  1,  0)
test$PropertyField11B  <-  ifelse(test$PropertyField11B  >  21,  1,  0)
test$PropertyField12  <-  ifelse(test$PropertyField12  <  4,  1,  0)
test$PropertyField13  <-  ifelse(test$PropertyField13  <  2,  1,  0)
test$PropertyField15  <-  ifelse(test$PropertyField15  >  7  
                                 &  test$PropertyField15  <=  11,  1,  0)  
test$PropertyField16A  <-  ifelse(test$PropertyField16A  >  4,  1,  0)
test$PropertyField18  <-  ifelse(test$PropertyField18 >  2  
                                 &  test$PropertyField18  <=  5,  1,  0)  
test$PropertyField19  <-  ifelse(test$PropertyField19  >  0  
                                 &  test$PropertyField18  <=  3,  1,  0)  
test$PropertyField20  <-  ifelse(test$PropertyField20  ==  0,  1,  0)
test$PropertyField21A  <-  ifelse(test$PropertyField21A  >  0  
                                  &  test$PropertyField21A  <=  8,  1,  0)  
test$PropertyField21B  <-  ifelse(test$PropertyField21B  <  15,  1,  0)
test$PropertyField22  <-  ifelse(test$PropertyField22  <  2,  1,  0)
test$PropertyField24A  <-  ifelse(test$PropertyField24A  >  0  
                                  &  test$PropertyField24A  <=  7,  1,  0)  
test$PropertyField24B  <-  ifelse(test$PropertyField24B  >  0  
                                  &  test$PropertyField24B  <=  15,  1,  0)  
test$PropertyField25  <-  ifelse(test$PropertyField25  >  1  
                                 &  test$PropertyField25  <=  2.5,  1,  0)  
test$PropertyField35  <-  ifelse(test$PropertyField35  ==  1,  1,  0)



#Modifying other variables
train$SalesField1A  <-  ifelse(train$SalesField1A  <=  5,  1,  0)  
train$SalesField1B  <-  ifelse(train$SalesField1B  <=  13,  1,  0)  
train$SalesField2A  <-  ifelse(train$SalesField2A  >  0  
                               &  train$SalesField2A  <=  7,  1,  0)  
train$SalesField2B  <-  ifelse(train$SalesField2B  >  0  
                               &  train$SalesField2B  <=  18,  1,  0)  
train$SalesField6  <-  ifelse(train$SalesField6  >  2  
                              &  train$SalesField6  <=  21,  1,  0)  
train$SalesField10  <-  ifelse(train$SalesField10  ==  0,  0,  1)
train$SalesField11  <-  ifelse(train$SalesField11  >  0  
                               &  train$SalesField11  <=  9,  1,  0)  
train$SalesField12  <-  ifelse(train$SalesField12  >  0  
                               &  train$SalesField12  <=  11,  1,  0)  
train$SalesField13  <-  ifelse(train$SalesField13  <  4,  1,  0)
train$SalesField14  <-  ifelse(train$SalesField14  <  4,  1,  0)
train$SalesField15  <-  ifelse(train$SalesField15  <  4,  1,  0)
train$Field7  <-  ifelse(train$Field7  >  1  &  train$Field7  <=  16  |  
                           train$Field7  >  23  &  train$Field7  <=  25,  1,  0)  
train$Field8  <-  ifelse(train$Field8  >  .98,  1,  0)
train$Field9  <-  ifelse(train$Field9  >  .00064,  1,  0)
train$Field10  <-  ifelse(train$Field10  <  1000,  1,  0)
train$Field11  <-  ifelse(train$Field11  <  1.2,  1,  0)
train$PropertyField1A  <-  ifelse(train$PropertyField1A  >  10,  1,  0)
train$PropertyField1B  <-  ifelse(train$PropertyField1B  >  14,  1,  0)
train$PropertyField9  <-  ifelse(train$PropertyField9  ==  0,  1,  0)
train$PropertyField10  <-  ifelse(train$PropertyField10  ==  1,  1,  0)
train$PropertyField11B  <-  ifelse(train$PropertyField11B  >  21,  1,  0)
train$PropertyField12  <-  ifelse(train$PropertyField12  <  4,  1,  0)
train$PropertyField13  <-  ifelse(train$PropertyField13  <  2,  1,  0)
train$PropertyField15  <-  ifelse(train$PropertyField15  >  7  
                                  &  train$PropertyField15  <=  11,  1,  0)  
train$PropertyField16A  <-  ifelse(train$PropertyField16A  >  4,  1,  0)
train$PropertyField18  <-  ifelse(train$PropertyField18 >  2  
                                  &  train$PropertyField18  <=  5,  1,  0)  
train$PropertyField19  <-  ifelse(train$PropertyField19  >  0  
                                  &  train$PropertyField18  <=  3,  1,  0)  
train$PropertyField20  <-  ifelse(train$PropertyField20  ==  0,  1,  0)
train$PropertyField21A  <-  ifelse(train$PropertyField21A  >  0  
                                   &  train$PropertyField21A  <=  8,  1,  0)  
train$PropertyField21B  <-  ifelse(train$PropertyField21B  <  15,  1,  0)
train$PropertyField22  <-  ifelse(train$PropertyField22  <  2,  1,  0)
train$PropertyField24A  <-  ifelse(train$PropertyField24A  >  0  
                                   &  train$PropertyField24A  <=  7,  1,  0)  
train$PropertyField24B  <-  ifelse(train$PropertyField24B  >  0  
                                   &  train$PropertyField24B  <=  15,  1,  0)  
train$PropertyField25  <-  ifelse(train$PropertyField25  >  1  
                                  &  train$PropertyField25  <=  2.5,  1,  0)  
train$PropertyField35  <-  ifelse(train$PropertyField35  ==  1,  1,  0)



#Iterations
train$PersonalField10A  <-  ifelse(train$PersonalField10A  >  0  
                                   &  train$PersonalField10A  <=  8,  10,  0)  
train$PersonalField12  <-  ifelse(train$PersonalField12  <  5,  10,  0)  
train$CoverageField11B  <-  ifelse(train$CoverageField11B  >  14,  10,  0)  
train$Field10  <-  ifelse(train$Field10  <  1000,  10,  0)
train$GeographicField12A  <-  ifelse(train$GeographicField12A  <  11,  10,  0)
train$SalesField1B  <-  ifelse(train$SalesField1B  <  12,  10,  0)
train$GeographicField12B  <-  ifelse(train$GeographicField12B  <  17,  10,  0)
train$SalesField2A  <-  ifelse(train$SalesField2A  >  0  
                               &  train$SalesField2A  <=  5,  1,  0)  
train$PersonalField4B  <-  ifelse(train$PersonalField4B  <  0,  0,  10)
train$PersonalField4A  <-  ifelse(train$PersonalField4A  <  0,  0,  10)
train$PropertyField25  <-  ifelse(train$PropertyField25  >  1  
                                  &  train$PropertyField25  <=  2.5,  10,  0)  
train$GeographicField9A  <-  ifelse(train$GeographicField9A  <  10,  10,  0)
train$PersonalField15  <-  ifelse(train$PersonalField15  <  12,  10,  0)
train$GeographicField13A  <-  ifelse(train$GeographicField13A  <  6,  10,  0)
train$GeographicField7A  <-  ifelse(train$GeographicField7A  <  11,  10,  0)
train$PersonalField82  <-  ifelse(train$PersonalField82  <  1,  0,  10)
train$GeographicField16A  <-  ifelse(train$GeographicField16A  <  8,  10,  0)
train$GeographicField11B  <-  ifelse(train$GeographicField11B  <  17,  10,  0)
train$GeographicField16B  <-  ifelse(train$GeographicField16B  >  3  
                                     &  train$GeographicField16B  <=  16,  1,  0)  
train$PersonalField26  <-  ifelse(train$PersonalField26  <  1,  0,  10)  
train$GeographicField9B  <-  ifelse(train$GeographicField9B  <  7,  10,  0)  

#Iterations
test$PersonalField10A  <-  ifelse(test$PersonalField10A  >  0  
                                  &  test$PersonalField10A  <=  8,  10,  0)  
test$PersonalField12  <-  ifelse(test$PersonalField12  <  5,  10,  0)  
test$CoverageField11B  <-  ifelse(test$CoverageField11B  >  14,  10,  0)  
test$Field10  <-  ifelse(test$Field10  <  1000,  10,  0)
test$GeographicField12A  <-  ifelse(test$GeographicField12A  <  11,  10,  0)
test$SalesField1B  <-  ifelse(test$SalesField1B  <  12,  10,  0)
test$GeographicField12B  <-  ifelse(test$GeographicField12B  <  17,  10,  0)
test$SalesField2A  <-  ifelse(test$SalesField2A  >  0  
                              &  test$SalesField2A  <=  5,  1,  0)  
test$PersonalField4B  <-  ifelse(test$PersonalField4B  <  0,  0,  10)
test$PersonalField4A  <-  ifelse(test$PersonalField4A  <  0,  0,  10)
test$PropertyField25  <-  ifelse(test$PropertyField25  >  1  
                                 &  test$PropertyField25  <=  2.5,  10,  0)  
test$GeographicField9A  <-  ifelse(test$GeographicField9A  <  10,  10,  0)
test$PersonalField15  <-  ifelse(test$PersonalField15  <  12,  10,  0)
test$GeographicField13A  <-  ifelse(test$GeographicField13A  <  6,  10,  0)
test$GeographicField7A  <-  ifelse(test$GeographicField7A  <  11,  10,  0)
test$PersonalField82  <-  ifelse(test$PersonalField82  <  1,  0,  10)
test$GeographicField16A  <-  ifelse(test$GeographicField16A  <  8,  10,  0)
test$GeographicField11B  <-  ifelse(test$GeographicField11B  <  17,  10,  0)
test$GeographicField16B  <-  ifelse(test$GeographicField16B  >  3  
                                    &  test$GeographicField16B  <=  16,  1,  0)  
test$PersonalField26  <-  ifelse(test$PersonalField26  <  1,  0,  10)  
test$GeographicField9B  <-  ifelse(test$GeographicField9B  <  7,  10,  0)  


#selecting only good predictors
good  <-  c("PropertyField37","SalesField5","PersonalField10A","PropertyField29")
good  <-  c(good,"Field7","SalesField4","PersonalField10B","PersonalField12")
good  <-  c(good,"CoverageField6B","SalesField1A","PersonalField13","CoverageField6A")
good  <-  c(good,"CoverageField11B","SalesField6","SalesField3","Field10","GeographicField12A")
good  <-  c(good,"PersonalField2","PersonalField1","PersonalField9","SalesField1B")
good  <-  c(good,"PropertyField35","PropertyField32","CoverageField11A","SalesField2B")
good  <-  c(good,"GeographicField12B","SalesField2A","PersonalField4B","GeographicField11A")
good  <-  c(good,"GeographicField7B","PersonalField4A","CoverageField3B","PropertyField25")
good  <-  c(good,'GeographicField6A','GeographicField13B','GeographicField8A','GeographicField9A')
good  <-  c(good,'PersonalField15','GeographicField13A','GeographicField7A','CoverageField3A')
good  <-  c(good,'PersonalField82','GeographicField16A','GeographicField11B','Field11')
good  <-  c(good,'GeographicField16B','PersonalField26','PropertyField8')
good  <-  c(good,'PropertyField15','GeographicField9B','Field9')


#Last Iteration
train$CoverageField1A  <-  ifelse(train$CoverageField1A  >  1  
                                  &  train$CoverageField1A  <  7,  1,  0)  
train$CoverageField1B  <-  ifelse(train$CoverageField1B  >  1  
                                  &  train$CoverageField1B  <  14,  1,  0)  
train$CoverageField2A  <-  ifelse(train$CoverageField2A  >  1  
                                  &  train$CoverageField2A  <  7,  1,  0)  
train$CoverageField2B  <-  ifelse(train$CoverageField2B  >  1  
                                  &  train$CoverageField2B  <  14,  1,  0)  
train$SalesField1A  <-  ifelse(train$SalesField1A  <=  5,  1,  0)  
train$SalesField1B  <-  ifelse(train$SalesField1B  <=  13,  1,  0)  
train$SalesField2A  <-  ifelse(train$SalesField2A  >  0  
                               &  train$SalesField2A  <=  7,  1,  0)  
train$SalesField2B  <-  ifelse(train$SalesField2B  >  0  
                               &  train$SalesField2B  <=  18,  1,  0)  
train$SalesField6  <-  ifelse(train$SalesField6  >  2  
                              &  train$SalesField6  <=  21,  1,  0)  
train$SalesField10  <-  ifelse(train$SalesField10  ==  0,  0,  1)
train$SalesField11  <-  ifelse(train$SalesField11  >  0  
                               &  train$SalesField11  <=  9,  1,  0)  
train$SalesField12  <-  ifelse(train$SalesField12  >  0  
                               &  train$SalesField12  <=  11,  1,  0)  
train$SalesField13  <-  ifelse(train$SalesField13  <  4,  1,  0)
train$SalesField14  <-  ifelse(train$SalesField14  <  4,  1,  0)
train$SalesField15  <-  ifelse(train$SalesField15  <  4,  1,  0)
train$PersonalField4B  <-  ifelse(train$PersonalField4B  <  0,  0,  10)
train$PersonalField4A  <-  ifelse(train$PersonalField4A  <  0,  0,  10)
train$PropertyField25  <-  ifelse(train$PropertyField25  >  1  
                                  &  train$PropertyField25  <=  2.5,  10,  0)  
train$GeographicField9A  <-  ifelse(train$GeographicField9A  <  10,  10,  0)
train$PersonalField15  <-  ifelse(train$PersonalField15  <  12,  10,  0)
train$GeographicField13A  <-  ifelse(train$GeographicField13A  <  6,  10,  0)
train$GeographicField7A  <-  ifelse(train$GeographicField7A  <  11,  10,  0)
train$PersonalField82  <-  ifelse(train$PersonalField82  <  1,  0,  10)
train$GeographicField16A  <-  ifelse(train$GeographicField16A  <  8,  10,  0)
train$GeographicField11B  <-  ifelse(train$GeographicField11B  <  17,  10,  0)
train$GeographicField16B  <-  ifelse(train$GeographicField16B  >  3  
                                     &  train$GeographicField16B  <=  16,  10,  0)  
train$PersonalField26  <-  ifelse(train$PersonalField26  <  1,  0,  10)  
train$GeographicField9B  <-  ifelse(train$GeographicField9B  <  7,  10, 0)
train$Field7  <-  ifelse(train$Field7  >  1  &  train$Field7  <=  16  |  
                           train$Field7  >  23  &  train$Field7  <=  25,  1,  0)  
train$Field8  <-  ifelse(train$Field8  >  .98,  1,  0)
train$Field9  <-  ifelse(train$Field9  >  .00064,  1,  0)
train$Field10  <-  ifelse(train$Field10  <  1000,  1,  0)
train$Field11  <-  ifelse(train$Field11  <  1.2,  1,  0)

#Logit on missing values
#selecting only good predictors
good  <-  c("PropertyField37","SalesField5","PersonalField10A")
good  <-  c(good,"Field7","SalesField4","PersonalField10B","PersonalField12")
good  <-  c(good,"CoverageField6B","SalesField1A","PersonalField13","CoverageField6A")
good  <-  c(good,"CoverageField11B","SalesField6","SalesField3","Field10","GeographicField12A")
good  <-  c(good,"PersonalField2","PersonalField1","PersonalField9","SalesField1B")
good  <-  c(good,"PropertyField35","PropertyField32","CoverageField11A","SalesField2B")
good  <-  c(good,"GeographicField12B","SalesField2A","PersonalField4B","GeographicField11A")
good  <-  c(good,"GeographicField7B","PersonalField4A","CoverageField3B","PropertyField25")
good  <-  c(good,'GeographicField6A','GeographicField13B','GeographicField8A','GeographicField9A')
good  <-  c(good,'PersonalField15','GeographicField13A','GeographicField7A','CoverageField3A')
good  <-  c(good,'PersonalField82','GeographicField16A','GeographicField11B','Field11')
good  <-  c(good,'GeographicField16B','PersonalField26','PropertyField8')
good  <-  c(good,'PropertyField15','GeographicField9B','Field9')


#------------------------#------------------------#------------------------
#-----------# Converting Categorical to Numeric variables #-----------------
#------------------------#------------------------#------------------------
categorical.test$data  <-  1  ;  categorical.train$data  <-  0
categorical  <-  rbind(categorical.test,  categorical.train)
rm(categorical.test,  categorical.train)  #Clearning space


categorical_to_numeric  <-  function(data)  {
  Output.df  <-  data.frame(row.names  =  1:dim(data)[1])
  for  (i  in  1:length(data))  {
    #creating a dataframe that stores all the values
    if  ((class(data[ ,  i])  ==  "character") | (class(data[,  i])  ==  "factor")){
      values  <-  unique(data[  ,  i])  ;  m  <-  data[  ,  i]
      df  <-  data.frame("X"  =  integer(),  "prop"  =  integer())
      for  (k  in  1  :  length(values))  {
        #Getting the number of 1s & 0's
        x_proportion  <-  length(m[m  ==  values[k]])  /  length(m)
        #appending the entry to the dataframe
        df  <-  rbind(df,  data.frame("X"  =  values[k],  "prop"  =  x_proportion))
      }#We have got the values for each categorical variable now
      M  <-  data.frame(m)  ;  names(M)  <-  "X"    
      res  <-  merge(M,  df,  by  =  "X")
      res  <-  data.frame(res[,  2])#final output for categorical columns
    }#End of if condition
    else  {res  <-  data.frame(data[  ,  i])}
    names(res)  <-  colnames(data[i])
    Output.df  <-  cbind(Output.df,  res)  #Binding to the dataframe
  }#End of Outer loop covering each predictor of the dataset
  Output.df
}#End of function

#Calling the function
categorical  <-  categorical_to_numeric(categorical)

#Splitting the dataset back
categorical.train  <-  categorical[categorical$data  ==  0,1:length(categorical)-1]  #Train
categorical.test  <-  categorical[categorical$data  ==  1,1:length(categorical)-1]  #Test
rm(categorical)


