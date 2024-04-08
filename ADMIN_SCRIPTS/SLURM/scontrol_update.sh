read -s -p "Enter Password for sudo: " sudo_pass

if [ -z "$2" ]; then #if no specific node, do all at the same time
        N_node=9  #master node out
        nodelist=()
        for (( i=0; i<=$N_node; i++ ))
        do
                nodenum=$i
                n=$(echo $i +5| bc)
                if [ $i -le 9 ]; then
                        nodenum=node0$i
                else
                        nodenum=node$i
                fi
                nodelist+=($nodenum)
        done
else #if specific node
        i=$2
        nodenum=$i
        n=$(echo $i +5| bc)
        if [ $i -le 9 ]; then
                nodenum=node0$i
        else
                nodenum=node$i
        fi
        nodelist=($nodenum)
fi

for node in ${nodelist[@]}; do
	echo $node
	echo $sudo_pass | sudo -S scontrol update nodename=$node state=resume
done
