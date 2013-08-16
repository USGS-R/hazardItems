clean.text	=	function(text){
	
	text	<-	sub('\\n\\n','',text,fixed = TRUE)
	text	<-	sub('\n','',text,fixed = TRUE)
	text	<-	sub('..','.',text,fixed = TRUE)
	text	<-	sub('. .','. ',text,fixed = TRUE)
	return(text)
}