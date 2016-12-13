##############################################################
## Configure followinng variables
##############################################################
destPath <- "/home/smita/MP/destinations.csv"

##############################################################
## Load data 
##############################################################
exp_destData = read.csv(destPath)

# Missing Values in the dataset corresponding to each predictor
na_count <-sapply(exp_destData, function(x) sum(length(which(is.na(x)))))
nas_dest = data.frame(na_count)

names(exp_destData)

## Exclude srch_destn_id
dest_num_feature = exp_destData[-1]

##############################################################
## Apply Pricipal Component to create new feature for the srch_dest
#############################################################

dest_pcs = princomp(dest_num_feature, cor = TRUE)

# using only three principal components

dest.dat = data.frame(dest_pcs$scores[, 1:3])
names(dest.dat) = c("dest_feature_pc1", "dest_feature_pc2", "dest_feature_pc3")
dest.dat = cbind(dest.dat, exp_destData$srch_destination_id)
write.csv(dest.dat, "/home/smita/MP/destination_pc.csv")
    
## Save scree plot
pdf('rplot.jpg')
screeplot(dest_pcs, npcs = 6, type = "lines", main = "Scree Plot of Principal Components" )
dev.off()

    
# remove unnecessory object and free memory
rm(dest_num_feature, dest_pcs, dest.dat, exp_destData, naa, nas_dest, dest)