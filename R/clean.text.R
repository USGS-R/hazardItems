clean.text	=	function(text){
	
	text	<-	sub('\\n\\n','',text)
	text	<-	sub('\n','',text)
	text	<-	sub('.. ','. ',text)
	text	<-	sub('. . ','. ',text)
	return(text)
}