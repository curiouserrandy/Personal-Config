FasdUAS 1.101.10   ��   ��    k             l     ��  r       	  I    �� 
��
�� .sysoexecTEXT���     TEXT 
 m         
echo $HOME   ��   	 o      ���� 0 hd  ��        l    ��  r        b        o    	���� 0 hd    m   	 
   0 *Config/local/blueboat/Bin/homesync.command     o      ���� 0 sync_cmd  ��        l     ������  ��        l    ��  r        I   �� ��
�� .sysoexecTEXT���     TEXT  m        echo /tmp/hsync.$$   ��    o      ���� 0 temp_file_base  ��        l    ��  r       !   b     " # " o    ���� 0 temp_file_base   # m     $ $ 
 .out    ! o      ���� 0 temp_file_out  ��     % & % l   ! '�� ' r    ! ( ) ( b     * + * o    ���� 0 temp_file_base   + m     , , 
 .err    ) o      ���� 0 temp_file_err  ��   &  - . - l     ������  ��   .  / 0 / l  " - 1�� 1 I  " -�� 2��
�� .ascrcmnt****      � **** 2 J   " ) 3 3  4 5 4 o   " #���� 0 hd   5  6 7 6 o   # $���� 0 sync_cmd   7  8 9 8 o   $ %���� 0 temp_file_base   9  : ; : o   % &���� 0 temp_file_out   ;  <�� < o   & '���� 0 temp_file_err  ��  ��  ��   0  = > = l     ������  ��   >  ? @ ? l     �� A��   A 2 , Mount script (copied from mount_script.app)    @  B C B l  . b D�� D O   . b E F E k   2 a G G  H I H Z   2 H J K���� J H   2 : L L E  2 9 M N M l  2 7 O�� O I  2 7������
�� .earslvolalis  P ��� null��  ��  ��   N m   7 8 P P  crosby    K I  = D�� Q��
�� .aevtmvolnull���     TEXT Q m   = @ R R  smb://192.168.1.10/crosby   ��  ��  ��   I  S�� S Z   I a T U���� T H   I S V V E  I R W X W l  I N Y�� Y I  I N������
�� .earslvolalis  P ��� null��  ��  ��   X m   N Q Z Z  randy    U I  V ]�� [��
�� .aevtmvolnull���     TEXT [ m   V Y \ \  smb://192.168.1.10/randy   ��  ��  ��  ��   F m   . / ] ]�null     ߀��  �
Finder.app���z����zbȿ��@�z�� ��   )       T(��� ���` �MACS   alis    r  Macintosh HD               �<XH+    �
Finder.app                                                       7߽�y        ����  	                CoreServices    ���      ���W      �  �  �  3Macintosh HD:System:Library:CoreServices:Finder.app    
 F i n d e r . a p p    M a c i n t o s h   H D  &System/Library/CoreServices/Finder.app  / ��  ��   C  ^ _ ^ l     ������  ��   _  ` a ` l  c � b�� b Q   c � c d e c I  f w�� f��
�� .sysoexecTEXT���     TEXT f b   f s g h g b   f q i j i b   f m k l k b   f k m n m o   f g���� 0 sync_cmd   n m   g j o o 	  >     l o   k l���� 0 temp_file_out   j m   m p p p 
  2>     h o   q r���� 0 temp_file_err  ��   d R      ���� q
�� .ascrerr ****      � ****��   q �� r��
�� 
errn r o      ���� 0 n  ��   e k    � s s  t u t r    � v w v I   ��� x��
�� .sysoexecTEXT���     TEXT x b    � y z y m    � { { 
 cat     z o   � ����� 0 temp_file_err  ��   w o      ���� 0 err_msg   u  | } | r   � � ~  ~ b   � � � � � b   � � � � � b   � � � � � m   � � � � # Sync script produced error #     � l  � � ��� � c   � � � � � o   � ����� 0 n   � m   � ���
�� 
TEXT��   � m   � � � �  :     � o   � ����� 0 err_msg    o      ���� 0 full_err_msg   }  ��� � I  � ��� ���
�� .sysodlogaskr        TEXT � o   � ����� 0 full_err_msg  ��  ��  ��   a  � � � l     ������  ��   �  ��� � l     ������  ��  ��       �� � ���   � ��
�� .aevtoappnull  �   � **** � �� ����� � ���
�� .aevtoappnull  �   � **** � k     � � �   � �   � �   � �   � �  % � �  / � �  B � �  `����  ��  ��   � ���� 0 n   �  ���� �� �� $�� ,������ ]�� P R�� Z \ o p�� � {�� ��� �����
�� .sysoexecTEXT���     TEXT�� 0 hd  �� 0 sync_cmd  �� 0 temp_file_base  �� 0 temp_file_out  �� 0 temp_file_err  �� 
�� .ascrcmnt****      � ****
�� .earslvolalis  P ��� null
�� .aevtmvolnull���     TEXT��   � ������
�� 
errn�� 0 n  ��  �� 0 err_msg  
�� 
TEXT�� 0 full_err_msg  
�� .sysodlogaskr        TEXT�� ��j E�O��%E�O�j E�O��%E�O��%E�O������vj O� 1*j � a j Y hO*j a  a j Y hUO �a %�%a %�%j W 2X  a �%j E` Oa �a &%a %_ %E` O_ j ascr  ��ޭ