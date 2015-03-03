<?

$end = array(0);
$lst = array(1, 10, array(1, 10, (array(1, 22, $end))));

# case xs of
#      nil => 0
#      cons(y, ys) => y + procList(ys)
      
function procList($xs) {
    switch($xs[0]) {
    case 0:
        return 0;
        break;
    case 1:
        $y = $xs[1];
        $ys = $xs[2];
        return $y + procList($ys);
    }    
}

echo procList($lst);
echo "\n";

?>
