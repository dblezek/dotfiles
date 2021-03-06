FasdUAS 1.101.10   ��   ��    k             l      ��  ��    9 3
	Move the current message to the "Handled" folder
     � 	 	 f 
 	 M o v e   t h e   c u r r e n t   m e s s a g e   t o   t h e   " H a n d l e d "   f o l d e r 
   
  
 l     ��������  ��  ��        i         I     �� ��
�� .aevtoappnull  �   � ****  J      ����  ��    k     �       l     ��������  ��  ��        l     ��������  ��  ��     ��  O     �    k    �       l   ��������  ��  ��        l   ��  ��    5 / get the currently selected message or messages     �     ^   g e t   t h e   c u r r e n t l y   s e l e c t e d   m e s s a g e   o r   m e s s a g e s   ! " ! r    	 # $ # 1    ��
�� 
CMgs $ o      ���� $0 selectedmessages selectedMessages "  % & % l  
 
��������  ��  ��   &  ' ( ' l  
 
�� ) *��   ) > 8 Check to make sure items are selected, if not then quit    * � + + p   C h e c k   t o   m a k e   s u r e   i t e m s   a r e   s e l e c t e d ,   i f   n o t   t h e n   q u i t (  , - , Z  
  . /���� . l  
  0���� 0 A   
  1 2 1 l  
  3���� 3 I  
 �� 4��
�� .corecnte****       **** 4 o   
 ���� $0 selectedmessages selectedMessages��  ��  ��   2 m    ���� ��  ��   / L    ����  ��  ��   -  5 6 5 l   ��������  ��  ��   6  7 8 7 X    � 9�� : 9 k   + � ; ;  < = < r   + 0 > ? > n   + . @ A @ 1   , .��
�� 
subj A o   + ,���� 0 
themessage 
theMessage ? o      ���� 0 thename theName =  B C B r   1 6 D E D n   1 4 F G F 1   2 4��
�� 
PlTC G o   1 2���� 0 
themessage 
theMessage E o      ���� 0 thebody theBody C  H I H r   7 ? J K J n   7 = L M L I   8 =�� N���� 0 	urlencode   N  O�� O o   8 9���� 0 thename theName��  ��   M  f   7 8 K o      ���� 0 	tasktitle 	taskTitle I  P Q P l  @ @�� R S��   R / ) set taskNote to urlencode(theBody) of me    S � T T R   s e t   t a s k N o t e   t o   u r l e n c o d e ( t h e B o d y )   o f   m e Q  U V U l  @ @�� W X��   W > 8 set taskNote to encode_text(theBody, true, false) of me    X � Y Y p   s e t   t a s k N o t e   t o   e n c o d e _ t e x t ( t h e B o d y ,   t r u e ,   f a l s e )   o f   m e V  Z [ Z l  @ @�� \ ]��   \ b \ Try with Python, according to https://discussions.apple.com/thread/1988268?start=0&tstart=0    ] � ^ ^ �   T r y   w i t h   P y t h o n ,   a c c o r d i n g   t o   h t t p s : / / d i s c u s s i o n s . a p p l e . c o m / t h r e a d / 1 9 8 8 2 6 8 ? s t a r t = 0 & t s t a r t = 0 [  _ ` _ l  @ @��������  ��  ��   `  a b a l  @ @��������  ��  ��   b  c d c l  @ @�� e f��   e 6 0 set theID to message id of theMessage as string    f � g g `   s e t   t h e I D   t o   m e s s a g e   i d   o f   t h e M e s s a g e   a s   s t r i n g d  h i h r   @ E j k j n   @ C l m l 1   A C��
�� 
ID   m o   @ A���� 0 
themessage 
theMessage k o      ���� 0 theid theID i  n o n r   F K p q p c   F I r s r o   F G���� 0 theid theID s m   G H��
�� 
TEXT q o      ���� 0 theid theID o  t u t l  L L��������  ��  ��   u  v w v r   L W x y x b   L U z { z b   L S | } | b   L O ~  ~ m   L M � � � � � : o u t l o o k t o t h i n g s : / / m e s s a g e ? i d =  o   M N���� 0 theid theID } m   O R � � � � �      { o   S T���� 0 thebody theBody y o      ���� 0 thebody theBody w  � � � r   X i � � � I  X e�� ���
�� .sysoexecTEXT���     TEXT � b   X a � � � m   X [ � � � � � � / u s r / b i n / p y t h o n   - c   ' i m p o r t   s y s ,   u r l l i b ;   p r i n t   u r l l i b . q u o t e ( s y s . a r g v [ 1 ] ) '   � n   [ ` � � � 1   \ `��
�� 
strq � o   [ \���� 0 thebody theBody��   � o      ���� 0 tasknote taskNote �  � � � l  j j��������  ��  ��   �  � � � r   j } � � � b   j y � � � b   j s � � � b   j o � � � m   j m � � � � � \ t w o d o : / / x - c a l l b a c k - u r l / a d d ? f o r L i s t = I n b o x & t a s k = � o   m n���� 0 	tasktitle 	taskTitle � m   o r � � � � �  & n o t e = � n   s x � � � o   t x���� 0 tasknote taskNote �  f   s t � o      ���� 0 theurl theURL �  � � � l  ~ ~�� � ���   � ( " display dialog theURL with icon 1    � � � � D   d i s p l a y   d i a l o g   t h e U R L   w i t h   i c o n   1 �  ��� � I  ~ ��� ���
�� .sysoexecTEXT���     TEXT � b   ~ � � � � b   ~ � � � � m   ~ � � � � � �  o p e n   - g   " � o   � ����� 0 theurl theURL � m   � � � � � � �  "��  ��  �� 0 
themessage 
theMessage : o    ���� $0 selectedmessages selectedMessages 8  � � � I  � ��� ���
�� .sysonotfnull��� ��� TEXT � b   � � � � � b   � � � � � m   � � � � � � �  C r e a t e d   � l  � � ����� � c   � � � � � l  � � ����� � I  � ��� ���
�� .corecnte****       **** � o   � ����� $0 selectedmessages selectedMessages��  ��  ��   � m   � ���
�� 
TEXT��  ��   � m   � � � � � � � &   t o   d o   i t e m s   i n   2 D o��   �  � � � l  � ���������  ��  ��   �  ��� � l  � ���������  ��  ��  ��    m      � �                                                                                  OPIM  alis    �  Macintosh HD               � �H+   	�OMicrosoft Outlook.app                                           	�̼�         ����  	                Microsoft Office 2011     � _      ̽"�     	�O   H  GMacintosh HD:Applications: Microsoft Office 2011: Microsoft Outlook.app   ,  M i c r o s o f t   O u t l o o k . a p p    M a c i n t o s h   H D  8Applications/Microsoft Office 2011/Microsoft Outlook.app  / ��  ��     � � � l     ��������  ��  ��   �  � � � l     �� � ���   � . ( this sub-routine is used to encode text    � � � � P   t h i s   s u b - r o u t i n e   i s   u s e d   t o   e n c o d e   t e x t �  � � � i     � � � I      �� ����� 0 encode_text   �  � � � o      ���� 0 	this_text   �  � � � o      ���� 0 encode_url_a encode_URL_A �  ��� � o      ���� 0 encode_url_b encode_URL_B��  ��   � k     f � �  � � � r      � � � m      � � � � � H a b c d e f g h i j k l m n o p q r s t u v w x y z 0 1 2 3 4 5 6 7 8 9 � l      ����� � o      ���� 0 standard_characters  ��  ��   �  � � � r     � � � m     � � � � � 2 $ + ! ' / ? ; & @ = # % > < { } [ ] " ~ ` ^ \ | * � l      ����� � o      ���� 0 url_a_chars URL_A_chars��  ��   �  � � � r     � � � m    	 � � � � �  . - _ : � l      ����� � o      ���� 0 url_b_chars URL_B_chars��  ��   �  � � � r     � � � l    ����� � o    �� 0 standard_characters  ��  ��   � l      ��~�} � o      �|�| 0 acceptable_characters  �~  �}   �  � � � Z    � ��{�z � =    � � � o    �y�y 0 encode_url_a encode_URL_A � m    �x
�x boovfals � r     � � � b     � � � l    ��w�v � o    �u�u 0 acceptable_characters  �w  �v   � l    ��t�s � o    �r�r 0 url_a_chars URL_A_chars�t  �s   � l      ��q�p � o      �o�o 0 acceptable_characters  �q  �p  �{  �z   �  � � � Z    / �n�m  =    # o     !�l�l 0 encode_url_b encode_URL_B m   ! "�k
�k boovfals r   & + b   & ) l  & '�j�i o   & '�h�h 0 acceptable_characters  �j  �i   l  ' (	�g�f	 o   ' (�e�e 0 url_b_chars URL_B_chars�g  �f   l     
�d�c
 o      �b�b 0 acceptable_characters  �d  �c  �n  �m   �  r   0 3 m   0 1 �   l     �a�` o      �_�_ 0 encoded_text  �a  �`    X   4 c�^ Z   D ^�] E  D G l  D E�\�[ o   D E�Z�Z 0 acceptable_characters  �\  �[   o   E F�Y�Y 0 	this_char   r   J O l  J M�X�W b   J M  l  J K!�V�U! o   J K�T�T 0 encoded_text  �V  �U    o   K L�S�S 0 	this_char  �X  �W   l     "�R�Q" o      �P�P 0 encoded_text  �R  �Q  �]   r   R ^#$# c   R \%&% l  R Z'�O�N' b   R Z()( l  R S*�M�L* o   R S�K�K 0 encoded_text  �M  �L  ) I   S Y�J+�I�J 0 encode_char  + ,�H, o   T U�G�G 0 	this_char  �H  �I  �O  �N  & m   Z [�F
�F 
TEXT$ l     -�E�D- o      �C�C 0 encoded_text  �E  �D  �^ 0 	this_char   o   7 8�B�B 0 	this_text   .�A. L   d f// l  d e0�@�?0 o   d e�>�> 0 encoded_text  �@  �?  �A   � 121 l     �=�<�;�=  �<  �;  2 343 i    565 I      �:7�9�: 0 encode_char  7 8�88 o      �7�7 0 	this_char  �8  �9  6 k     K99 :;: r     <=< l    >�6�5> l    ?�4�3? I    �2@�1
�2 .sysoctonshor       TEXT@ o     �0�0 0 	this_char  �1  �4  �3  �6  �5  = l     A�/�.A o      �-�- 0 	ascii_num 	ASCII_num�/  �.  ; BCB r     DED J    FF GHG m    	II �JJ  0H KLK m   	 
MM �NN  1L OPO m   
 QQ �RR  2P STS m    UU �VV  3T WXW m    YY �ZZ  4X [\[ m    ]] �^^  5\ _`_ m    aa �bb  6` cdc m    ee �ff  7d ghg m    ii �jj  8h klk m    mm �nn  9l opo m    qq �rr  Ap sts m    uu �vv  Bt wxw m    yy �zz  Cx {|{ m    }} �~~  D| � m    �� ���  E� ��,� m    �� ���  F�,  E l     ��+�*� o      �)�) 0 hex_list  �+  �*  C ��� r   ! /��� n   ! -��� 4   " -�(�
�( 
cobj� l  % ,��'�&� [   % ,��� l  % *��%�$� _   % *��� o   % &�#�# 0 	ascii_num 	ASCII_num� m   & )�"�" �%  �$  � m   * +�!�! �'  �&  � l  ! "�� �� o   ! "�� 0 hex_list  �   �  � o      �� 0 x  � ��� r   0 >��� n   0 <��� 4   1 <��
� 
cobj� l  4 ;���� [   4 ;��� l  4 9���� `   4 9��� o   4 5�� 0 	ascii_num 	ASCII_num� m   5 8�� �  �  � m   9 :�� �  �  � l  0 1���� o   0 1�� 0 hex_list  �  �  � o      �� 0 y  � ��� L   ? K�� c   ? J��� l  ? F���� b   ? F��� b   ? D��� m   ? B�� ���  %� o   B C�� 0 x  � o   D E�� 0 y  �  �  � m   F I�
� 
TEXT�  4 ��� l     �
�	��
  �	  �  � ��� i    ��� I      ���� 0 	urlencode  � ��� o      �� 0 thetext theText�  �  � k    �� ��� r     ��� m     �� ���  � o      �� 0 
thetextenc 
theTextEnc� ��� X    ���� k    ��� ��� r    ��� o    �� 0 eachchar eachChar� o      � �  0 usechar useChar� ��� r    !��� I   �����
�� .sysoctonshor       TEXT� o    ���� 0 eachchar eachChar��  � o      ���� 0 eachcharnum eachCharNum� ��� Z   " ������� =   " %��� o   " #���� 0 eachcharnum eachCharNum� m   # $����  � r   ( +��� m   ( )�� ���  % 2 0� o      ���� 0 usechar useChar� ��� F   . ��� F   . k��� F   . Y��� F   . I��� F   . 9��� l  . 1������ >   . 1��� o   . /���� 0 eachcharnum eachCharNum� m   / 0���� *��  ��  � l  4 7������ >   4 7��� o   4 5���� 0 eachcharnum eachCharNum� m   5 6���� _��  ��  � l  < G������ G   < G��� A   < ?��� o   < =���� 0 eachcharnum eachCharNum� m   = >���� -� ?   B E��� o   B C���� 0 eachcharnum eachCharNum� m   C D���� .��  ��  � l  L W������ G   L W��� A   L O��� o   L M���� 0 eachcharnum eachCharNum� m   M N���� 0� ?   R U��� o   R S���� 0 eachcharnum eachCharNum� m   S T���� 9��  ��  � l  \ i������ G   \ i��� A   \ _��� o   \ ]���� 0 eachcharnum eachCharNum� m   ] ^���� A� ?   b g��� o   b c���� 0 eachcharnum eachCharNum� m   c f���� Z��  ��  � l  n }������ G   n }��� A   n s   o   n o���� 0 eachcharnum eachCharNum m   o r���� a� ?   v { o   v w���� 0 eachcharnum eachCharNum m   w z���� z��  ��  � �� k   � �  r   � �	 I  � ���

�� .sysorondlong        doub
 l  � ����� ^   � � o   � ����� 0 eachcharnum eachCharNum m   � ����� ��  ��   ����
�� 
dire m   � ���
�� olierndD��  	 o      ���� 0 firstdig firstDig  r   � � `   � � o   � ����� 0 eachcharnum eachCharNum m   � �����  o      ���� 0 	seconddig 	secondDig  Z   � ����� ?   � � o   � ����� 0 firstdig firstDig m   � ����� 	 k   � �  r   � �  [   � �!"! o   � ����� 0 firstdig firstDig" m   � ����� 7  o      ���� 0 anum aNum #��# r   � �$%$ I  � ���&��
�� .sysontocTEXT       shor& o   � ����� 0 anum aNum��  % o      ���� 0 firstdig firstDig��  ��  ��   '(' Z   � �)*����) ?   � �+,+ o   � ����� 0 	seconddig 	secondDig, m   � ����� 	* k   � �-- ./. r   � �010 [   � �232 o   � ����� 0 	seconddig 	secondDig3 m   � ����� 71 o      ���� 0 anum aNum/ 4��4 r   � �565 I  � ���7��
�� .sysontocTEXT       shor7 o   � ����� 0 anum aNum��  6 o      ���� 0 	seconddig 	secondDig��  ��  ��  ( 898 r   � �:;: c   � �<=< l  � �>����> b   � �?@? b   � �ABA m   � �CC �DD  %B l  � �E����E c   � �FGF o   � ����� 0 firstdig firstDigG m   � ���
�� 
TEXT��  ��  @ l  � �H����H c   � �IJI o   � ����� 0 	seconddig 	secondDigJ m   � ���
�� 
TEXT��  ��  ��  ��  = m   � ���
�� 
TEXT; o      ���� 0 numhex numHex9 K��K r   � �LML o   � ����� 0 numhex numHexM o      ���� 0 usechar useChar��  ��  ��  � N��N r   � �OPO c   � �QRQ b   � �STS o   � ����� 0 
thetextenc 
theTextEncT o   � ����� 0 usechar useCharR m   � ���
�� 
TEXTP o      ���� 0 
thetextenc 
theTextEnc��  � 0 eachchar eachChar� n    
UVU 2   
��
�� 
cha V o    ���� 0 thetext theText� W��W L  XX o  ���� 0 
thetextenc 
theTextEnc��  � Y��Y l     ��������  ��  ��  ��       ��Z[\]^��  Z ��������
�� .aevtoappnull  �   � ****�� 0 encode_text  �� 0 encode_char  �� 0 	urlencode  [ �� ����_`��
�� .aevtoappnull  �   � ****��  ��  _ ���� 0 
themessage 
theMessage`  ���������������������������� � � ��~�}�| � ��{ � � � ��z
�� 
CMgs�� $0 selectedmessages selectedMessages
�� .corecnte****       ****
�� 
kocl
�� 
cobj
�� 
subj�� 0 thename theName
�� 
PlTC�� 0 thebody theBody�� 0 	urlencode  �� 0 	tasktitle 	taskTitle
�� 
ID  �� 0 theid theID
� 
TEXT
�~ 
strq
�} .sysoexecTEXT���     TEXT�| 0 tasknote taskNote�{ 0 theurl theURL
�z .sysonotfnull��� ��� TEXT�� �� �*�,E�O�j k hY hO v�[��l kh  ��,E�O��,E�O)�k+ 
E�O��,E�O��&E�O��%a %�%E�Oa �a ,%j E` Oa �%a %)a ,%E` Oa _ %a %j [OY��Oa �j �&%a %j OPU\ �y ��x�wab�v�y 0 encode_text  �x �uc�u c  �t�s�r�t 0 	this_text  �s 0 encode_url_a encode_URL_A�r 0 encode_url_b encode_URL_B�w  a 	�q�p�o�n�m�l�k�j�i�q 0 	this_text  �p 0 encode_url_a encode_URL_A�o 0 encode_url_b encode_URL_B�n 0 standard_characters  �m 0 url_a_chars URL_A_chars�l 0 url_b_chars URL_B_chars�k 0 acceptable_characters  �j 0 encoded_text  �i 0 	this_char  b 	 � � ��h�g�f�e�d
�h 
kocl
�g 
cobj
�f .corecnte****       ****�e 0 encode_char  
�d 
TEXT�v g�E�O�E�O�E�O�E�O�f  
��%E�Y hO�f  
��%E�Y hO�E�O .�[��l kh �� 
��%E�Y �*�k+ %�&E�[OY��O�] �c6�b�ade�`�c 0 encode_char  �b �_f�_ f  �^�^ 0 	this_char  �a  d �]�\�[�Z�Y�] 0 	this_char  �\ 0 	ascii_num 	ASCII_num�[ 0 hex_list  �Z 0 x  �Y 0 y  e �XIMQUY]aeimquy}���W�V��U
�X .sysoctonshor       TEXT�W 
�V 
cobj
�U 
TEXT�` L�j  E�O���������������a a vE�O�a �a "k/E�O�a �a #k/E�Oa �%�%a &^ �T��S�Rgh�Q�T 0 	urlencode  �S �Pi�P i  �O�O 0 thetext theText�R  g 	�N�M�L�K�J�I�H�G�F�N 0 thetext theText�M 0 
thetextenc 
theTextEnc�L 0 eachchar eachChar�K 0 usechar useChar�J 0 eachcharnum eachCharNum�I 0 firstdig firstDig�H 0 	seconddig 	secondDig�G 0 anum aNum�F 0 numhex numHexh ��E�D�C�B�A�@��?�>�=�<�;�:�9�8�7�6�5�4�3�2�1�0�/�.C�-
�E 
cha 
�D 
kocl
�C 
cobj
�B .corecnte****       ****
�A .sysoctonshor       TEXT�@  �? *�> _
�= 
bool�< -�; .�: 0�9 9�8 A�7 Z�6 a�5 z�4 
�3 
dire
�2 olierndD
�1 .sysorondlong        doub�0 	�/ 7
�. .sysontocTEXT       shor
�- 
TEXT�Q�E�O ���-[��l kh �E�O�j E�O��  �E�Y Ť�	 ���&	 ��
 ���&�&	 ��
 ���&�&	 ��
 	�a �&�&	 �a 
 	�a �&�& p�a !a a l E�O�a #E�O�a  �a E�O�j E�Y hO�a  �a E�O�j E�Y hOa �a &%�a &%a &E�O�E�Y hO��%a &E�[OY�O� ascr  ��ޭ