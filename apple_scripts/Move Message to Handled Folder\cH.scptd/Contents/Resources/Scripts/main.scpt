FasdUAS 1.101.10   ��   ��    k             l      ��  ��    9 3
	Move the current message to the "Handled" folder
     � 	 	 f 
 	 M o v e   t h e   c u r r e n t   m e s s a g e   t o   t h e   " H a n d l e d "   f o l d e r 
   
  
 l     ��������  ��  ��     ��  i         I     �� ��
�� .aevtoappnull  �   � ****  J      ����  ��    k     W       l     ��������  ��  ��     ��  O     W    k    V       l   ��������  ��  ��        l   ��  ��    5 / get the currently selected message or messages     �   ^   g e t   t h e   c u r r e n t l y   s e l e c t e d   m e s s a g e   o r   m e s s a g e s      r    	   !   1    ��
�� 
CMgs ! o      ���� $0 selectedmessages selectedMessages   " # " l  
 
��������  ��  ��   #  $ % $ l  
 
�� & '��   & > 8 Check to make sure items are selected, if not then quit    ' � ( ( p   C h e c k   t o   m a k e   s u r e   i t e m s   a r e   s e l e c t e d ,   i f   n o t   t h e n   q u i t %  ) * ) Z  
  + ,���� + l  
  -���� - A   
  . / . l  
  0���� 0 I  
 �� 1��
�� .corecnte****       **** 1 o   
 ���� $0 selectedmessages selectedMessages��  ��  ��   / m    ���� ��  ��   , L    ����  ��  ��   *  2 3 2 l   ��������  ��  ��   3  4 5 4 r    " 6 7 6 n      8 9 8 1     ��
�� 
pInb 9 1    ��
�� 
dfAc 7 o      ���� 0 	thefolder 	theFolder 5  : ; : l  # #��������  ��  ��   ;  < = < l  # #�� > ?��   > 8 2 set theFolder to folder "Handled" of inbox folder    ? � @ @ d   s e t   t h e F o l d e r   t o   f o l d e r   " H a n d l e d "   o f   i n b o x   f o l d e r =  A B A r   # ( C D C n   # & E F E 1   $ &��
�� 
pnam F o   # $���� 0 	thefolder 	theFolder D o      ���� 0 thename theName B  G H G l  ) )�� I J��   I 3 - set theParent to name of parent of theFolder    J � K K Z   s e t   t h e P a r e n t   t o   n a m e   o f   p a r e n t   o f   t h e F o l d e r H  L M L r   ) / N O N n   ) - P Q P 4   * -�� R
�� 
cFld R m   + , S S � T T  H a n d l e d Q o   ) *���� 0 	thefolder 	theFolder O o      ���� 0 handled   M  U V U l  0 0�� W X��   W Z T display dialog "Found folders... " & theName & " to " & name of handled with icon 1    X � Y Y �   d i s p l a y   d i a l o g   " F o u n d   f o l d e r s . . .   "   &   t h e N a m e   &   "   t o   "   &   n a m e   o f   h a n d l e d   w i t h   i c o n   1 V  Z [ Z l  0 0��������  ��  ��   [  \ ] \ l  0 0�� ^ _��   ^ ' ! set theSelection to �class pusl�    _ � ` ` B   s e t   t h e S e l e c t i o n   t o   � c l a s s   p u s l � ]  a b a l  0 0�� c d��   c 6 0 set theCurrentMessage to item 1 of theSelection    d � e e `   s e t   t h e C u r r e n t M e s s a g e   t o   i t e m   1   o f   t h e S e l e c t i o n b  f g f l  0 0�� h i��   h A ; set theMessage to incoming message ID of theCurrentMessage    i � j j v   s e t   t h e M e s s a g e   t o   i n c o m i n g   m e s s a g e   I D   o f   t h e C u r r e n t M e s s a g e g  k l k l  0 0��������  ��  ��   l  m n m l  0 0�� o p��   o ( " move theCurrentMessage to handled    p � q q D   m o v e   t h e C u r r e n t M e s s a g e   t o   h a n d l e d n  r s r l  0 0�� t u��   t 1 + repeat with theMessage in selectedMessages    u � v v V   r e p e a t   w i t h   t h e M e s s a g e   i n   s e l e c t e d M e s s a g e s s  w x w X   0 T y�� z y k   @ O { {  | } | l  @ @�� ~ ��   ~ > 8		repeat with i from (count selectedMessages) to 1 by -1     � � � p 	 	 r e p e a t   w i t h   i   f r o m   ( c o u n t   s e l e c t e d M e s s a g e s )   t o   1   b y   - 1 }  � � � l  @ @�� � ���   � 1 +			set theMessage to item i of theSelection    � � � � V 	 	 	 s e t   t h e M e s s a g e   t o   i t e m   i   o f   t h e S e l e c t i o n �  � � � r   @ E � � � n   @ C � � � 1   A C��
�� 
subj � o   @ A���� 0 
themessage 
theMessage � o      ���� 0 thename theName �  � � � l  F F�� � ���   � < 6 display dialog "Moved message " & theName with icon 1    � � � � l   d i s p l a y   d i a l o g   " M o v e d   m e s s a g e   "   &   t h e N a m e   w i t h   i c o n   1 �  � � � I  F M�� � �
�� .coremovenull���     obj  � o   F G���� 0 
themessage 
theMessage � �� ���
�� 
insh � o   H I���� 0 handled  ��   �  ��� � l  N N�� � ���   � F @ set storage of theMessage to folder "Handled" of folder "Inbox"    � � � � �   s e t   s t o r a g e   o f   t h e M e s s a g e   t o   f o l d e r   " H a n d l e d "   o f   f o l d e r   " I n b o x "��  �� 0 
themessage 
theMessage z o   3 4���� $0 selectedmessages selectedMessages x  � � � l  U U��������  ��  ��   �  ��� � l  U U��������  ��  ��  ��    m      � �                                                                                  OPIM  alis    �  Macintosh HD               ����H+   -Microsoft Outlook.app                                           :M�=5        ����  	                Microsoft Office 2011     ��O      ���     -   �  GMacintosh HD:Applications: Microsoft Office 2011: Microsoft Outlook.app   ,  M i c r o s o f t   O u t l o o k . a p p    M a c i n t o s h   H D  8Applications/Microsoft Office 2011/Microsoft Outlook.app  / ��  ��  ��       
�� � � � � � ���������   � ����������������
�� .aevtoappnull  �   � ****�� $0 selectedmessages selectedMessages�� 0 	thefolder 	theFolder�� 0 thename theName�� 0 handled  ��  ��  ��   � �� ���� � ���
�� .aevtoappnull  �   � ****��  ��   � ���� 0 
themessage 
theMessage �  ������������������� S������������
�� 
CMgs�� $0 selectedmessages selectedMessages
�� .corecnte****       ****
�� 
dfAc
�� 
pInb�� 0 	thefolder 	theFolder
�� 
pnam�� 0 thename theName
�� 
cFld�� 0 handled  
�� 
kocl
�� 
cobj
�� 
subj
�� 
insh
�� .coremovenull���     obj �� X� T*�,E�O�j k hY hO*�,�,E�O��,E�O���/E�O #�[��l kh  ��,E�O���l OP[OY��OPU � �� ���  �   � �  � �  �������
�� 
inm ��  �
�� kfrmID   �  � �  �������
�� 
cMFo��	q
�� kfrmID   � � � � , R E :   s a m p l e n a m e s   b a c k u p �  � �  �������
�� 
cMFo��	x
�� kfrmID  ��  ��  ��   ascr  ��ޭ