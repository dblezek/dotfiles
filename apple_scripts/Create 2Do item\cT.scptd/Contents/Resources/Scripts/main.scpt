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
 ���� $0 selectedmessages selectedMessages��  ��  ��   2 m    ���� ��  ��   / L    ����  ��  ��   -  5 6 5 l   ��������  ��  ��   6  7 8 7 X    r 9�� : 9 k   + m ; ;  < = < r   + 0 > ? > n   + . @ A @ 1   , .��
�� 
subj A o   + ,���� 0 
themessage 
theMessage ? o      ���� 0 thename theName =  B C B r   1 6 D E D n   1 4 F G F 1   2 4��
�� 
PlTC G o   1 2���� 0 
themessage 
theMessage E o      ���� 0 thebody theBody C  H I H r   7 ? J K J n   7 = L M L I   8 =�� N���� 0 	urlencode   N  O�� O o   8 9���� 0 thename theName��  ��   M  f   7 8 K o      ���� 0 	tasktitle 	taskTitle I  P Q P l  @ @�� R S��   R / ) set taskNote to urlencode(theBody) of me    S � T T R   s e t   t a s k N o t e   t o   u r l e n c o d e ( t h e B o d y )   o f   m e Q  U V U l  @ @�� W X��   W > 8 set taskNote to encode_text(theBody, true, false) of me    X � Y Y p   s e t   t a s k N o t e   t o   e n c o d e _ t e x t ( t h e B o d y ,   t r u e ,   f a l s e )   o f   m e V  Z [ Z l  @ @�� \ ]��   \ b \ Try with Python, according to https://discussions.apple.com/thread/1988268?start=0&tstart=0    ] � ^ ^ �   T r y   w i t h   P y t h o n ,   a c c o r d i n g   t o   h t t p s : / / d i s c u s s i o n s . a p p l e . c o m / t h r e a d / 1 9 8 8 2 6 8 ? s t a r t = 0 & t s t a r t = 0 [  _ ` _ r   @ K a b a I  @ I�� c��
�� .sysoexecTEXT���     TEXT c b   @ E d e d m   @ A f f � g g � / u s r / b i n / p y t h o n   - c   ' i m p o r t   s y s ,   u r l l i b ;   p r i n t   u r l l i b . q u o t e ( s y s . a r g v [ 1 ] ) '   e n   A D h i h 1   B D��
�� 
strq i o   A B���� 0 thebody theBody��   b o      ���� 0 tasknote taskNote `  j k j r   L ] l m l b   L Y n o n b   L U p q p b   L Q r s r m   L O t t � u u \ t w o d o : / / x - c a l l b a c k - u r l / a d d ? f o r L i s t = I n b o x & t a s k = s o   O P���� 0 	tasktitle 	taskTitle q m   Q T v v � w w  & n o t e = o n   U X x y x o   V X���� 0 tasknote taskNote y  f   U V m o      ���� 0 theurl theURL k  z { z l  ^ ^�� | }��   | ( " display dialog theURL with icon 1    } � ~ ~ D   d i s p l a y   d i a l o g   t h e U R L   w i t h   i c o n   1 {  ��  I  ^ m�� ���
�� .sysoexecTEXT���     TEXT � b   ^ i � � � b   ^ e � � � m   ^ a � � � � �  o p e n   - g   " � o   a d���� 0 theurl theURL � m   e h � � � � �  "��  ��  �� 0 
themessage 
theMessage : o    ���� $0 selectedmessages selectedMessages 8  � � � I  s ��� ���
�� .sysonotfnull��� ��� TEXT � b   s � � � � b   s � � � � m   s v � � � � �  C r e a t e d   � l  v  ����� � c   v  � � � l  v { ����� � I  v {�� ���
�� .corecnte****       **** � o   v w���� $0 selectedmessages selectedMessages��  ��  ��   � m   { ~��
�� 
TEXT��  ��   � m   � � � � � � � &   t o   d o   i t e m s   i n   2 D o��   �  � � � l  � ���������  ��  ��   �  ��� � l  � ���������  ��  ��  ��    m      � �                                                                                  OPIM  alis    �  Macintosh HD               ����H+   -Microsoft Outlook.app                                           :M�=5        ����  	                Microsoft Office 2011     ��O      ���     -   �  GMacintosh HD:Applications: Microsoft Office 2011: Microsoft Outlook.app   ,  M i c r o s o f t   O u t l o o k . a p p    M a c i n t o s h   H D  8Applications/Microsoft Office 2011/Microsoft Outlook.app  / ��  ��     � � � l     ��������  ��  ��   �  � � � l     �� � ���   � . ( this sub-routine is used to encode text    � � � � P   t h i s   s u b - r o u t i n e   i s   u s e d   t o   e n c o d e   t e x t �  � � � i     � � � I      �� ����� 0 encode_text   �  � � � o      ���� 0 	this_text   �  � � � o      ���� 0 encode_url_a encode_URL_A �  ��� � o      ���� 0 encode_url_b encode_URL_B��  ��   � k     f � �  � � � r      � � � m      � � � � � H a b c d e f g h i j k l m n o p q r s t u v w x y z 0 1 2 3 4 5 6 7 8 9 � l      ����� � o      ���� 0 standard_characters  ��  ��   �  � � � r     � � � m     � � � � � 2 $ + ! ' / ? ; & @ = # % > < { } [ ] " ~ ` ^ \ | * � l      ����� � o      ���� 0 url_a_chars URL_A_chars��  ��   �  � � � r     � � � m    	 � � � � �  . - _ : � l      ����� � o      ���� 0 url_b_chars URL_B_chars��  ��   �  � � � r     � � � l    ����� � o    ���� 0 standard_characters  ��  ��   � l      ����� � o      ���� 0 acceptable_characters  ��  ��   �  � � � Z    � ����� � =    � � � o    ���� 0 encode_url_a encode_URL_A � m    ��
�� boovfals � r     � � � b     � � � l    ����� � o    ���� 0 acceptable_characters  ��  ��   � l    ����� � o    ���� 0 url_a_chars URL_A_chars��  ��   � l      ����� � o      ���� 0 acceptable_characters  ��  ��  ��  ��   �  � � � Z    / � ����� � =    # � � � o     !���� 0 encode_url_b encode_URL_B � m   ! "��
�� boovfals � r   & + � � � b   & ) � � � l  & ' ���� � o   & '�~�~ 0 acceptable_characters  ��  �   � l  ' ( ��}�| � o   ' (�{�{ 0 url_b_chars URL_B_chars�}  �|   � l      ��z�y � o      �x�x 0 acceptable_characters  �z  �y  ��  ��   �  � � � r   0 3 � � � m   0 1 � � � � �   � l      ��w�v � o      �u�u 0 encoded_text  �w  �v   �  � � � X   4 c ��t � � Z   D ^ � ��s � � E  D G � � � l  D E ��r�q � o   D E�p�p 0 acceptable_characters  �r  �q   � o   E F�o�o 0 	this_char   � r   J O � � � l  J M ��n�m � b   J M � � � l  J K ��l�k � o   J K�j�j 0 encoded_text  �l  �k   � o   K L�i�i 0 	this_char  �n  �m   � l      ��h�g � o      �f�f 0 encoded_text  �h  �g  �s   � r   R ^ � � � c   R \ � � � l  R Z �e�d  b   R Z l  R S�c�b o   R S�a�a 0 encoded_text  �c  �b   I   S Y�`�_�` 0 encode_char   �^ o   T U�]�] 0 	this_char  �^  �_  �e  �d   � m   Z [�\
�\ 
TEXT � l     �[�Z o      �Y�Y 0 encoded_text  �[  �Z  �t 0 	this_char   � o   7 8�X�X 0 	this_text   � �W L   d f l  d e	�V�U	 o   d e�T�T 0 encoded_text  �V  �U  �W   � 

 l     �S�R�Q�S  �R  �Q    i     I      �P�O�P 0 encode_char   �N o      �M�M 0 	this_char  �N  �O   k     K  r      l    �L�K l    �J�I I    �H�G
�H .sysoctonshor       TEXT o     �F�F 0 	this_char  �G  �J  �I  �L  �K   l     �E�D o      �C�C 0 	ascii_num 	ASCII_num�E  �D    r      J      !  m    	"" �##  0! $%$ m   	 
&& �''  1% ()( m   
 ** �++  2) ,-, m    .. �//  3- 010 m    22 �33  41 454 m    66 �77  55 898 m    :: �;;  69 <=< m    >> �??  7= @A@ m    BB �CC  8A DED m    FF �GG  9E HIH m    JJ �KK  AI LML m    NN �OO  BM PQP m    RR �SS  CQ TUT m    VV �WW  DU XYX m    ZZ �[[  EY \�B\ m    ]] �^^  F�B   l     _�A�@_ o      �?�? 0 hex_list  �A  �@   `a` r   ! /bcb n   ! -ded 4   " -�>f
�> 
cobjf l  % ,g�=�<g [   % ,hih l  % *j�;�:j _   % *klk o   % &�9�9 0 	ascii_num 	ASCII_numl m   & )�8�8 �;  �:  i m   * +�7�7 �=  �<  e l  ! "m�6�5m o   ! "�4�4 0 hex_list  �6  �5  c o      �3�3 0 x  a non r   0 >pqp n   0 <rsr 4   1 <�2t
�2 
cobjt l  4 ;u�1�0u [   4 ;vwv l  4 9x�/�.x `   4 9yzy o   4 5�-�- 0 	ascii_num 	ASCII_numz m   5 8�,�, �/  �.  w m   9 :�+�+ �1  �0  s l  0 1{�*�){ o   0 1�(�( 0 hex_list  �*  �)  q o      �'�' 0 y  o |�&| L   ? K}} c   ? J~~ l  ? F��%�$� b   ? F��� b   ? D��� m   ? B�� ���  %� o   B C�#�# 0 x  � o   D E�"�" 0 y  �%  �$   m   F I�!
�! 
TEXT�&   ��� l     � ���   �  �  � ��� i    ��� I      ���� 0 	urlencode  � ��� o      �� 0 thetext theText�  �  � k    �� ��� r     ��� m     �� ���  � o      �� 0 
thetextenc 
theTextEnc� ��� X    ���� k    ��� ��� r    ��� o    �� 0 eachchar eachChar� o      �� 0 usechar useChar� ��� r    !��� I   ���
� .sysoctonshor       TEXT� o    �� 0 eachchar eachChar�  � o      �� 0 eachcharnum eachCharNum� ��� Z   " ������ =   " %��� o   " #�� 0 eachcharnum eachCharNum� m   # $��  � r   ( +��� m   ( )�� ���  % 2 0� o      �� 0 usechar useChar� ��� F   . ��� F   . k��� F   . Y��� F   . I��� F   . 9��� l  . 1���� >   . 1��� o   . /�� 0 eachcharnum eachCharNum� m   / 0�
�
 *�  �  � l  4 7��	�� >   4 7��� o   4 5�� 0 eachcharnum eachCharNum� m   5 6�� _�	  �  � l  < G���� G   < G��� A   < ?��� o   < =�� 0 eachcharnum eachCharNum� m   = >�� -� ?   B E��� o   B C�� 0 eachcharnum eachCharNum� m   C D� �  .�  �  � l  L W������ G   L W��� A   L O��� o   L M���� 0 eachcharnum eachCharNum� m   M N���� 0� ?   R U��� o   R S���� 0 eachcharnum eachCharNum� m   S T���� 9��  ��  � l  \ i������ G   \ i��� A   \ _��� o   \ ]���� 0 eachcharnum eachCharNum� m   ] ^���� A� ?   b g��� o   b c���� 0 eachcharnum eachCharNum� m   c f���� Z��  ��  � l  n }������ G   n }��� A   n s��� o   n o���� 0 eachcharnum eachCharNum� m   o r���� a� ?   v {��� o   v w���� 0 eachcharnum eachCharNum� m   w z���� z��  ��  � ���� k   � ��� ��� r   � ���� I  � �����
�� .sysorondlong        doub� l  � ������� ^   � ���� o   � ����� 0 eachcharnum eachCharNum� m   � ����� ��  ��  � �����
�� 
dire� m   � ���
�� olierndD��  � o      ���� 0 firstdig firstDig� ��� r   � ���� `   � ���� o   � ����� 0 eachcharnum eachCharNum� m   � ����� � o      ���� 0 	seconddig 	secondDig� ��� Z   � �������� ?   � ���� o   � ����� 0 firstdig firstDig� m   � ����� 	� k   � ��� ��� r   � ���� [   � ���� o   � ����� 0 firstdig firstDig� m   � ����� 7� o      ���� 0 anum aNum� ���� r   � ���� I  � ������
�� .sysontocTEXT       shor� o   � ����� 0 anum aNum��  � o      ���� 0 firstdig firstDig��  ��  ��  �    Z   � ����� ?   � � o   � ����� 0 	seconddig 	secondDig m   � ����� 	 k   � �  r   � �	
	 [   � � o   � ����� 0 	seconddig 	secondDig m   � ����� 7
 o      ���� 0 anum aNum �� r   � � I  � �����
�� .sysontocTEXT       shor o   � ����� 0 anum aNum��   o      ���� 0 	seconddig 	secondDig��  ��  ��    r   � � c   � � l  � ����� b   � � b   � � m   � � �  % l  � ����� c   � �  o   � ����� 0 firstdig firstDig  m   � ���
�� 
TEXT��  ��   l  � �!����! c   � �"#" o   � ����� 0 	seconddig 	secondDig# m   � ���
�� 
TEXT��  ��  ��  ��   m   � ���
�� 
TEXT o      ���� 0 numhex numHex $��$ r   � �%&% o   � ����� 0 numhex numHex& o      ���� 0 usechar useChar��  ��  �  � '��' r   � �()( c   � �*+* b   � �,-, o   � ����� 0 
thetextenc 
theTextEnc- o   � ����� 0 usechar useChar+ m   � ���
�� 
TEXT) o      ���� 0 
thetextenc 
theTextEnc��  � 0 eachchar eachChar� n    
./. 2   
��
�� 
cha / o    ���� 0 thetext theText� 0��0 L  11 o  ���� 0 
thetextenc 
theTextEnc��  � 2��2 l     ��������  ��  ��  ��       ��3456789:;<=��������������  3 ��������������������������������
�� .aevtoappnull  �   � ****�� 0 encode_text  �� 0 encode_char  �� 0 	urlencode  �� $0 selectedmessages selectedMessages�� 0 thename theName�� 0 thebody theBody�� 0 	tasktitle 	taskTitle�� 0 tasknote taskNote�� 0 theurl theURL��  ��  ��  ��  ��  ��  4 �� ����>?��
�� .aevtoappnull  �   � ****��  ��  > ���� 0 
themessage 
theMessage?  ����������������������� f������ t v�� � � ��� ���
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
strq
�� .sysoexecTEXT���     TEXT�� 0 tasknote taskNote�� 0 theurl theURL
�� 
TEXT
�� .sysonotfnull��� ��� TEXT�� �� �*�,E�O�j k hY hO V�[��l kh  ��,E�O��,E�O)�k+ 
E�O���,%j E�Oa �%a %)�,%E` Oa _ %a %j [OY��Oa �j a &%a %j OPU5 � ��~�}@A�|� 0 encode_text  �~ �{B�{ B  �z�y�x�z 0 	this_text  �y 0 encode_url_a encode_URL_A�x 0 encode_url_b encode_URL_B�}  @ 	�w�v�u�t�s�r�q�p�o�w 0 	this_text  �v 0 encode_url_a encode_URL_A�u 0 encode_url_b encode_URL_B�t 0 standard_characters  �s 0 url_a_chars URL_A_chars�r 0 url_b_chars URL_B_chars�q 0 acceptable_characters  �p 0 encoded_text  �o 0 	this_char  A 	 � � � ��n�m�l�k�j
�n 
kocl
�m 
cobj
�l .corecnte****       ****�k 0 encode_char  
�j 
TEXT�| g�E�O�E�O�E�O�E�O�f  
��%E�Y hO�f  
��%E�Y hO�E�O .�[��l kh �� 
��%E�Y �*�k+ %�&E�[OY��O�6 �i�h�gCD�f�i 0 encode_char  �h �eE�e E  �d�d 0 	this_char  �g  C �c�b�a�`�_�c 0 	this_char  �b 0 	ascii_num 	ASCII_num�a 0 hex_list  �` 0 x  �_ 0 y  D �^"&*.26:>BFJNRVZ]�]�\��[
�^ .sysoctonshor       TEXT�] 
�\ 
cobj
�[ 
TEXT�f L�j  E�O���������������a a vE�O�a �a "k/E�O�a �a #k/E�Oa �%�%a &7 �Z��Y�XFG�W�Z 0 	urlencode  �Y �VH�V H  �U�U 0 thetext theText�X  F 	�T�S�R�Q�P�O�N�M�L�T 0 thetext theText�S 0 
thetextenc 
theTextEnc�R 0 eachchar eachChar�Q 0 usechar useChar�P 0 eachcharnum eachCharNum�O 0 firstdig firstDig�N 0 	seconddig 	secondDig�M 0 anum aNum�L 0 numhex numHexG ��K�J�I�H�G�F��E�D�C�B�A�@�?�>�=�<�;�:�9�8�7�6�5�4�3
�K 
cha 
�J 
kocl
�I 
cobj
�H .corecnte****       ****
�G .sysoctonshor       TEXT�F  �E *�D _
�C 
bool�B -�A .�@ 0�? 9�> A�= Z�< a�; z�: 
�9 
dire
�8 olierndD
�7 .sysorondlong        doub�6 	�5 7
�4 .sysontocTEXT       shor
�3 
TEXT�W�E�O ���-[��l kh �E�O�j E�O��  �E�Y Ť�	 ���&	 ��
 ���&�&	 ��
 ���&�&	 ��
 	�a �&�&	 �a 
 	�a �&�& p�a !a a l E�O�a #E�O�a  �a E�O�j E�Y hO�a  �a E�O�j E�Y hOa �a &%�a &%a &E�O�E�Y hO��%a &E�[OY�O�8 �2I�2 I  JJ KK  ��1�0�/
�1 
inm �0  �
�/ kfrmID  9 �LL V A A I S S   2 0 1 6   p l a n n i n g   R e t r e a t   p o t e n t i a l   g o a l s: �MM
� F e l l o w   A A I S S   l e a d e r s ,  I   h o p e   y o u   a l l   h a d   a   w o n d e r f u l   t i m e   o f f   o v e r   t h e   h o l i d a y s   a n d   g o t   l o t s   o f   r e s t   a n d   r e j u v e n a t i o n   (   I   k n o w   I   d i d ) .       I   w a n t   t o   f i r s t   t h a n k   y o u   a l l   f o r   p a r t i c i p a t i n g   i n   t h e   A A I S S   p l a n n i n g   r e t r e a t   l a s t   m o n t h .     I   h o p e   y o u   f o u n d   i t   w a s   v a l u a b l e 
   u s e   o f   y o u r   t i m e   a n d   p r o v i d e   y o u   s o m e   i n s i g h t   t o   t h e   o p p o r t u n i t i e s   a n d   c h a l l e n g e s   w e   a r e   f a c i n g   i n   2 0 1 6 .     T h e   m a i n   g o a l   o f   t h e   r e t r e a t   w a s   t o   c o m e   u p   w i t h   3 - 4   g o a l s / a r e a s   o f   f o c u s   t h a t   w e   a s   l e a d e r s   w i l l   c o m m i t   t o   a c c o m p l i s h i n g   i n   2 0 1 6 .     D u r i n g   t h e   b r a i n s t o r m i n g 
   t h e   g r o u p   h a d   c o m e   u p   w i t h   1 0   p o t e n t i a l   g o a l s ,   s o   I   n e e d   y o u r   i n p u t   t o   w h i t t l e   t h i s   d o w n   t o   3 .     T h e   i d e a   i s   t h a t   o n c e   w e   a g r e e   u p o n   3   t o   4   g o a l s   w e   w i l l   e s t a b l i s h   t a s k   f o r c e s   a r o u n d   t h e s e   a c t i v i t y   w i t h   a   p r i m a r y   s p o n s o r .     T h i s   t e a m   w i l l   b e   r e s p o n s i b l e   f o r   d e f i n i n g ,   s c o p i n g   a n d   i m p l e m e n t i n g   s o l u t i o n s   t o   a d d r e s s   t h e   g o a l s .     S o   I  v e 
   a t t a c h e d   w h a t   w a s   w r i t t e n   o n   t h e   w h i t e   b o a r d   a n d   I   n e e d   y o u r   t o p   t h r e e   v o t e s .     G i v e n   t h a t ,   p l e a s e   r e v i e w   a n d   s e n d   b a c k   y o u   v o t e   i n   t h e   f o l l o w i n g   f o r m a t :     1 .             
 Q u a l i t y   A s s u r a n c e  2 .             
 W o r k l o a d   m a n a g e m e n t  3 .             
 S t a f f i n g   S t r a t e g i e s     P l e a s e   r e v i e w   a n d   h a v e   y o u   v o t e s   s e n t   t o   m e   b y   W e d n e s d a y ,   1 3 t h .       T h e n   I  l l   c o m p i l e   t h e n   a n d   s e n d   o u t   t h e   r e s u l t s .     A l s o ,   i f   y o u   a r e   i n t e r e s t e d   i n   l e a d i n g   o n e   o f   t h e s e   i n i t i a t i v e s   p l e a s e   l e t   m e   k n o w .     T h a n k   y o u   a g a i n   f o r   s h a p i n g   o u r   s t r a t e g y !     J a s o n  ; �NN j A A I S S % 2 0 2 0 1 6 % 2 0 p l a n n i n g % 2 0 R e t r e a t % 2 0 p o t e n t i a l % 2 0 g o a l s< �OOF F e l l o w % 2 0 A A I S S % 2 0 l e a d e r s % 2 C % 0 D I % 2 0 h o p e % 2 0 y o u % 2 0 a l l % 2 0 h a d % 2 0 a % 2 0 w o n d e r f u l % 2 0 t i m e % 2 0 o f f % 2 0 o v e r % 2 0 t h e % 2 0 h o l i d a y s % 2 0 a n d % 2 0 g o t % 2 0 l o t s % 2 0 o f % 2 0 r e s t % 2 0 a n d % 2 0 r e j u v e n a t i o n % 2 0 % 2 8 % 2 0 I % 2 0 k n o w % 2 0 I % 2 0 d i d % 2 9 . % 2 0 % 2 0 % 2 0 I % 2 0 w a n t % 2 0 t o % 2 0 f i r s t % 2 0 t h a n k % 2 0 y o u % 2 0 a l l % 2 0 f o r % 2 0 p a r t i c i p a t i n g % 2 0 i n % 2 0 t h e % 2 0 A A I S S % 2 0 p l a n n i n g % 2 0 r e t r e a t % 2 0 l a s t % 2 0 m o n t h . % 2 0 % 2 0 I % 2 0 h o p e % 2 0 y o u % 2 0 f o u n d % 2 0 i t % 2 0 w a s % 2 0 v a l u a b l e % 0 A % 2 0 u s e % 2 0 o f % 2 0 y o u r % 2 0 t i m e % 2 0 a n d % 2 0 p r o v i d e % 2 0 y o u % 2 0 s o m e % 2 0 i n s i g h t % 2 0 t o % 2 0 t h e % 2 0 o p p o r t u n i t i e s % 2 0 a n d % 2 0 c h a l l e n g e s % 2 0 w e % 2 0 a r e % 2 0 f a c i n g % 2 0 i n % 2 0 2 0 1 6 . % 2 0 % 2 0 T h e % 2 0 m a i n % 2 0 g o a l % 2 0 o f % 2 0 t h e % 2 0 r e t r e a t % 2 0 w a s % 2 0 t o % 2 0 c o m e % 2 0 u p % 2 0 w i t h % 2 0 3 - 4 % 2 0 g o a l s / a r e a s % 2 0 o f % 2 0 f o c u s % 2 0 t h a t % 2 0 w e % 2 0 a s % 2 0 l e a d e r s % 2 0 w i l l % 2 0 c o m m i t % 2 0 t o % 2 0 a c c o m p l i s h i n g % 2 0 i n % 2 0 2 0 1 6 . % 2 0 % 2 0 D u r i n g % 2 0 t h e % 2 0 b r a i n s t o r m i n g % 0 A % 2 0 t h e % 2 0 g r o u p % 2 0 h a d % 2 0 c o m e % 2 0 u p % 2 0 w i t h % 2 0 1 0 % 2 0 p o t e n t i a l % 2 0 g o a l s % 2 C % 2 0 s o % 2 0 I % 2 0 n e e d % 2 0 y o u r % 2 0 i n p u t % 2 0 t o % 2 0 w h i t t l e % 2 0 t h i s % 2 0 d o w n % 2 0 t o % 2 0 3 . % 0 D % 2 0 % 0 D T h e % 2 0 i d e a % 2 0 i s % 2 0 t h a t % 2 0 o n c e % 2 0 w e % 2 0 a g r e e % 2 0 u p o n % 2 0 3 % 2 0 t o % 2 0 4 % 2 0 g o a l s % 2 0 w e % 2 0 w i l l % 2 0 e s t a b l i s h % 2 0 t a s k % 2 0 f o r c e s % 2 0 a r o u n d % 2 0 t h e s e % 2 0 a c t i v i t y % 2 0 w i t h % 2 0 a % 2 0 p r i m a r y % 2 0 s p o n s o r . % 2 0 % 2 0 T h i s % 2 0 t e a m % 2 0 w i l l % 2 0 b e % 2 0 r e s p o n s i b l e % 2 0 f o r % 2 0 d e f i n i n g % 2 C % 2 0 s c o p i n g % 2 0 a n d % 2 0 i m p l e m e n t i n g % 2 0 s o l u t i o n s % 2 0 t o % 2 0 a d d r e s s % 2 0 t h e % 2 0 g o a l s . % 2 0 % 2 0 S o % 2 0 I % E 2 % 8 0 % 9 9 v e % 0 A % 2 0 a t t a c h e d % 2 0 w h a t % 2 0 w a s % 2 0 w r i t t e n % 2 0 o n % 2 0 t h e % 2 0 w h i t e % 2 0 b o a r d % 2 0 a n d % 2 0 I % 2 0 n e e d % 2 0 y o u r % 2 0 t o p % 2 0 t h r e e % 2 0 v o t e s . % 2 0 % 2 0 G i v e n % 2 0 t h a t % 2 C % 2 0 p l e a s e % 2 0 r e v i e w % 2 0 a n d % 2 0 s e n d % 2 0 b a c k % 2 0 y o u % 2 0 v o t e % 2 0 i n % 2 0 t h e % 2 0 f o l l o w i n g % 2 0 f o r m a t % 3 A % 0 D % 2 0 % 0 D 1 . % 2 0 % 2 0 % 2 0 % 2 0 % 2 0 % 2 0 % 0 A Q u a l i t y % 2 0 A s s u r a n c e % 0 D 2 . % 2 0 % 2 0 % 2 0 % 2 0 % 2 0 % 2 0 % 0 A W o r k l o a d % 2 0 m a n a g e m e n t % 0 D 3 . % 2 0 % 2 0 % 2 0 % 2 0 % 2 0 % 2 0 % 0 A S t a f f i n g % 2 0 S t r a t e g i e s % 0 D % 2 0 % 0 D P l e a s e % 2 0 r e v i e w % 2 0 a n d % 2 0 h a v e % 2 0 y o u % 2 0 v o t e s % 2 0 s e n t % 2 0 t o % 2 0 m e % 2 0 b y % 2 0 W e d n e s d a y % 2 C % 2 0 1 3 t h . % 2 0 % 2 0 % 2 0 T h e n % 2 0 I % E 2 % 8 0 % 9 9 l l % 2 0 c o m p i l e % 2 0 t h e n % 2 0 a n d % 2 0 s e n d % 2 0 o u t % 2 0 t h e % 2 0 r e s u l t s . % 2 0 % 2 0 A l s o % 2 C % 2 0 i f % 2 0 y o u % 2 0 a r e % 2 0 i n t e r e s t e d % 2 0 i n % 2 0 l e a d i n g % 2 0 o n e % 2 0 o f % 2 0 t h e s e % 2 0 i n i t i a t i v e s % 2 0 p l e a s e % 2 0 l e t % 2 0 m e % 2 0 k n o w . % 0 D % 2 0 % 0 D T h a n k % 2 0 y o u % 2 0 a g a i n % 2 0 f o r % 2 0 s h a p i n g % 2 0 o u r % 2 0 s t r a t e g y % 2 1 % 0 D % 2 0 % 0 D J a s o n % 0 D % 0 D= �PP t w o d o : / / x - c a l l b a c k - u r l / a d d ? f o r L i s t = I n b o x & t a s k = A A I S S % 2 0 2 0 1 6 % 2 0 p l a n n i n g % 2 0 R e t r e a t % 2 0 p o t e n t i a l % 2 0 g o a l s & n o t e = F e l l o w % 2 0 A A I S S % 2 0 l e a d e r s % 2 C % 0 D I % 2 0 h o p e % 2 0 y o u % 2 0 a l l % 2 0 h a d % 2 0 a % 2 0 w o n d e r f u l % 2 0 t i m e % 2 0 o f f % 2 0 o v e r % 2 0 t h e % 2 0 h o l i d a y s % 2 0 a n d % 2 0 g o t % 2 0 l o t s % 2 0 o f % 2 0 r e s t % 2 0 a n d % 2 0 r e j u v e n a t i o n % 2 0 % 2 8 % 2 0 I % 2 0 k n o w % 2 0 I % 2 0 d i d % 2 9 . % 2 0 % 2 0 % 2 0 I % 2 0 w a n t % 2 0 t o % 2 0 f i r s t % 2 0 t h a n k % 2 0 y o u % 2 0 a l l % 2 0 f o r % 2 0 p a r t i c i p a t i n g % 2 0 i n % 2 0 t h e % 2 0 A A I S S % 2 0 p l a n n i n g % 2 0 r e t r e a t % 2 0 l a s t % 2 0 m o n t h . % 2 0 % 2 0 I % 2 0 h o p e % 2 0 y o u % 2 0 f o u n d % 2 0 i t % 2 0 w a s % 2 0 v a l u a b l e % 0 A % 2 0 u s e % 2 0 o f % 2 0 y o u r % 2 0 t i m e % 2 0 a n d % 2 0 p r o v i d e % 2 0 y o u % 2 0 s o m e % 2 0 i n s i g h t % 2 0 t o % 2 0 t h e % 2 0 o p p o r t u n i t i e s % 2 0 a n d % 2 0 c h a l l e n g e s % 2 0 w e % 2 0 a r e % 2 0 f a c i n g % 2 0 i n % 2 0 2 0 1 6 . % 2 0 % 2 0 T h e % 2 0 m a i n % 2 0 g o a l % 2 0 o f % 2 0 t h e % 2 0 r e t r e a t % 2 0 w a s % 2 0 t o % 2 0 c o m e % 2 0 u p % 2 0 w i t h % 2 0 3 - 4 % 2 0 g o a l s / a r e a s % 2 0 o f % 2 0 f o c u s % 2 0 t h a t % 2 0 w e % 2 0 a s % 2 0 l e a d e r s % 2 0 w i l l % 2 0 c o m m i t % 2 0 t o % 2 0 a c c o m p l i s h i n g % 2 0 i n % 2 0 2 0 1 6 . % 2 0 % 2 0 D u r i n g % 2 0 t h e % 2 0 b r a i n s t o r m i n g % 0 A % 2 0 t h e % 2 0 g r o u p % 2 0 h a d % 2 0 c o m e % 2 0 u p % 2 0 w i t h % 2 0 1 0 % 2 0 p o t e n t i a l % 2 0 g o a l s % 2 C % 2 0 s o % 2 0 I % 2 0 n e e d % 2 0 y o u r % 2 0 i n p u t % 2 0 t o % 2 0 w h i t t l e % 2 0 t h i s % 2 0 d o w n % 2 0 t o % 2 0 3 . % 0 D % 2 0 % 0 D T h e % 2 0 i d e a % 2 0 i s % 2 0 t h a t % 2 0 o n c e % 2 0 w e % 2 0 a g r e e % 2 0 u p o n % 2 0 3 % 2 0 t o % 2 0 4 % 2 0 g o a l s % 2 0 w e % 2 0 w i l l % 2 0 e s t a b l i s h % 2 0 t a s k % 2 0 f o r c e s % 2 0 a r o u n d % 2 0 t h e s e % 2 0 a c t i v i t y % 2 0 w i t h % 2 0 a % 2 0 p r i m a r y % 2 0 s p o n s o r . % 2 0 % 2 0 T h i s % 2 0 t e a m % 2 0 w i l l % 2 0 b e % 2 0 r e s p o n s i b l e % 2 0 f o r % 2 0 d e f i n i n g % 2 C % 2 0 s c o p i n g % 2 0 a n d % 2 0 i m p l e m e n t i n g % 2 0 s o l u t i o n s % 2 0 t o % 2 0 a d d r e s s % 2 0 t h e % 2 0 g o a l s . % 2 0 % 2 0 S o % 2 0 I % E 2 % 8 0 % 9 9 v e % 0 A % 2 0 a t t a c h e d % 2 0 w h a t % 2 0 w a s % 2 0 w r i t t e n % 2 0 o n % 2 0 t h e % 2 0 w h i t e % 2 0 b o a r d % 2 0 a n d % 2 0 I % 2 0 n e e d % 2 0 y o u r % 2 0 t o p % 2 0 t h r e e % 2 0 v o t e s . % 2 0 % 2 0 G i v e n % 2 0 t h a t % 2 C % 2 0 p l e a s e % 2 0 r e v i e w % 2 0 a n d % 2 0 s e n d % 2 0 b a c k % 2 0 y o u % 2 0 v o t e % 2 0 i n % 2 0 t h e % 2 0 f o l l o w i n g % 2 0 f o r m a t % 3 A % 0 D % 2 0 % 0 D 1 . % 2 0 % 2 0 % 2 0 % 2 0 % 2 0 % 2 0 % 0 A Q u a l i t y % 2 0 A s s u r a n c e % 0 D 2 . % 2 0 % 2 0 % 2 0 % 2 0 % 2 0 % 2 0 % 0 A W o r k l o a d % 2 0 m a n a g e m e n t % 0 D 3 . % 2 0 % 2 0 % 2 0 % 2 0 % 2 0 % 2 0 % 0 A S t a f f i n g % 2 0 S t r a t e g i e s % 0 D % 2 0 % 0 D P l e a s e % 2 0 r e v i e w % 2 0 a n d % 2 0 h a v e % 2 0 y o u % 2 0 v o t e s % 2 0 s e n t % 2 0 t o % 2 0 m e % 2 0 b y % 2 0 W e d n e s d a y % 2 C % 2 0 1 3 t h . % 2 0 % 2 0 % 2 0 T h e n % 2 0 I % E 2 % 8 0 % 9 9 l l % 2 0 c o m p i l e % 2 0 t h e n % 2 0 a n d % 2 0 s e n d % 2 0 o u t % 2 0 t h e % 2 0 r e s u l t s . % 2 0 % 2 0 A l s o % 2 C % 2 0 i f % 2 0 y o u % 2 0 a r e % 2 0 i n t e r e s t e d % 2 0 i n % 2 0 l e a d i n g % 2 0 o n e % 2 0 o f % 2 0 t h e s e % 2 0 i n i t i a t i v e s % 2 0 p l e a s e % 2 0 l e t % 2 0 m e % 2 0 k n o w . % 0 D % 2 0 % 0 D T h a n k % 2 0 y o u % 2 0 a g a i n % 2 0 f o r % 2 0 s h a p i n g % 2 0 o u r % 2 0 s t r a t e g y % 2 1 % 0 D % 2 0 % 0 D J a s o n % 0 D % 0 D��  ��  ��  ��  ��  ��   ascr  ��ޭ