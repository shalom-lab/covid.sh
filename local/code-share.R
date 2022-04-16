load('./data.rda')

write.csv(case.asym.wider,'./share/case.asym.wider.csv',row.names = F)
write.csv(case.asym.wider.sh,'./share/case.asym.wider.sh.csv',row.names = F)
write.csv(map.2.new,'./share/map.2.new.csv',row.names = F)
