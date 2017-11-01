# ������ �������� �������

���� ���� �������� �������� � ���������� ���� ����������� ���� �����. ������
�����������:
_������ ������� � ���� ������, 401-�_

## ���������

- [����������� ��������� �������������](#�����������-���������-�������������)
  - [K ��������� ������� (KNN)](#k-���������-�������-knn)
  - [K ���������� ��������� ������� (KNN)](#k-����������-���������-�������-knn)
  - [������������ ���� (PW)](#������������-����-pw)

## ����������� ��������� �������������

����������� ��������� ������������� �������� �� __�������� ������������__,
������� ������� � ���, ��� <u>������ �������� ������������� ������ ������</u>.

�������, ������������ "��������" �������� �������� __����� ��������__.
��� ������� ������������ ��������� �������:

![](http://latex.codecogs.com/svg.latex?%5Clarge%20%5Crho%3A%20%28X%20%5Ctimes%20X%29%20%5Crightarrow%20%5Cmathbb%7BR%7D)
(������� ����������)

��� ���������� ������ ����� �������� �� ������� �������� ��������.

### K ��������� ������� (KNN)

��� ������ �������� ������� _u_ � ������ _y_ �������� ���������� ���������
�������:
![](http://latex.codecogs.com/svg.latex?%5Clarge%20W%28i%2C%20u%29%20%3D%20%5Bi%20%5Cleq%20k%5D)
, ��� _i_ ���������� ������� ������ �� ���������� � ����� _u_.

������� �������, �������� �������� _k_ ��������� ������� � ����������
��� �����, ������� ����� ��������� ����������� ������� ���������� ���.

���������� ��������� �������� ��
[������](oRRRlova/KNN.R)

#### ������ ���������

������������ �������� �� ������� ������ ������:

![LOO ��� KNN...](IMG/KNN.png)

�������� ������ ���� ���������� ��� ��������� _k_, �������� ������ ���� �
���������� ��������, ������ ��� ����� _k_, ������� � _k > 95_, ������
������������ ������. ��� ����������� ���, ��� ������� ������ ��������
![](http://latex.codecogs.com/svg.latex?%5Clarge%20W%28i%2C%20u%29%20%3D%20%5Bi%20%5Cleq%20k%5D)
����� �� ��������� ������� ���������, � ��������� ���� �� �������. ��-��
����� �� �������, ��� ��������� ����� ������ �� ����� _u_ ������ ��
������������� � ����� �� �����, ��� � �������, ����������� �
���������������� ��������.

__�����:__
- ����� � ����������
- �������� ���������� ��� ��������� ���������� _k_

__������:__
- ���������� ������� ��� ������� �������
- ������������� ����� ��������
![](http://latex.codecogs.com/svg.latex?%5Clarge%20O%28n%20%5Clog%20n%29)
, ��� ��� ������� ���������� ����� �� ����������
- ������ ����� ����������
- ����������� ������ ��������
- � ������ ���������� ����� ������� �������� �������� �����
- �� ��� ����� � ���������� ����������� ����� �������

### K ���������� ��������� ������� (KNN)

��� ������ �������� ������� _u_ � ������ _y_ �������� ���������� ���������
�������:
![](http://latex.codecogs.com/svg.latex?%5Clarge%20W%28i%2C%20u%29%20%3D%20%5Bi%20%5Cleq%20k%5D%20w%28i%29)
, ��� _i_ ���������� ������� ������ �� ���������� � ����� _u_, � 
![](http://latex.codecogs.com/svg.latex?%5Clarge%20w%28i%29) � ������
��������� ������� ����. ��������� �������� __����������� KNN__ ����������
�� __KNN__.

�� �� ����� ��������� ��������� ������� ����:
![](http://latex.codecogs.com/svg.latex?%5Clarge%20w%28i%29%20%3D%20%5Cfrac%7Bk%20&plus;%201%20-%20i%7D%7Bk%7D)

���������� ��������� �������� ��
[������](oRRRlova/kwKNN.R)

#### ������ ���������

������������ �������� �� ������� ������ ������:

![LOO ��� ����������� KNN...](IMG/kwKNN.png)

�������� ������ ���� ���������� ��� ����� _k_, ���������� ������������ ������
� ����������� ����� ���� � ���� ��������.

�������� __���������� k �������__ ����������� ���������� �� ��������
__KNN__ ���, ��� <u>��������� ������� ��������</u> ��� �������������. ���
�����, ������� � ����� _u_ ������� ����� ������ �� ��� ������� �������, ���
�������. ������ ��-�� ���� �����������, ��� ������� _k_ ������� �����
������ ��������������, ������� �������
![](http://latex.codecogs.com/svg.latex?%5Clarge%20w%28i%29)
������� �������� ���������.

__�����:__
- ����� � ����������
- �������� ���������� ��� ����� _k_

__������:__
- ���������� ������� ��� ������� �������
- ������������� ����� ��������
![](http://latex.codecogs.com/svg.latex?%5Clarge%20O%28n%20%5Clog%20n%29)
, ��� ��� ������� ���������� ����� �� ����������
- ������ ����� ����������
- � ������ ���������� ����� ������� �������� �������� �����
(������ ����� ��������, ��� ��� ������ ����� ����������� ������ �����)
- �� ��� ����� � ���������� ����������� ����� �������

### ������������ ���� (PW)

��� ������ �������� ������� _u_ � ������ _y_ �������� ���������� ���������
�������:

![](http://latex.codecogs.com/svg.latex?%5Clarge%20W%28i%2C%20u%29%20%3D%20K%28%5Cfrac%7B%5Crho%28u%2C%20x%5Ei_u%29%7D%7Bh%7D%29)
, ��� 
![](http://latex.codecogs.com/svg.latex?%5Clarge%20K%28z%29) � ������� ����.

���� ����� ����������� 4 ���� ����:
- ������������� ![](http://latex.codecogs.com/svg.latex?%5Clarge%20R%28z%29%20%3D%20%5Cfrac%7B1%7D%7B2%7D%20%5B%7Cz%7C%20%5Cleq%201%5D)
- ����������� ![](http://latex.codecogs.com/svg.latex?%5Clarge%20T%28z%29%20%3D%20%281%20-%20%7Cz%7C%29%20%5Ccdot%20%5B%7Cz%7C%20%5Cleq%201%5D)
- ������������ ![](http://latex.codecogs.com/svg.latex?%5Clarge%20Q%28z%29%20%3D%20%5Cfrac%7B15%7D%7B16%7D%20%281%20-%20z%5E2%29%5E2%20%5Ccdot%20%5B%7Cz%7C%20%5Cleq%201%5D)
- ������������ ![](http://latex.codecogs.com/svg.latex?%5Clarge%20E%28z%29%20%3D%20%5Cfrac%7B3%7D%7B4%7D%20%281%20-%20z%5E2%29%20%5Ccdot%20%5B%7Cz%7C%20%5Cleq%201%5D)

�� � ��������� ����� ��������� ���� ����������, ������ �� �������� ������������� ���
������ �����.

__�������� �������:__ �������� ��� ���������������� ����� _u_ ������
����������, �������� _h_. ��� �����, �� �������� � ��� ����������,
����������� (���� ��� ������� �� ����). ��� ���������, ����������� ���,
�����������, � ����� � ���������� ����� ��������� �����������.

���������� ��������� �������� ��
[������](oRRRlova/PW.R)

#### ������ ���������

������������ �������� �� ������� ������ ������:

_������������� ����:_
![LOO ��� PW...](IMG/PW_R.png)

_����������� ����:_
![LOO ��� PW...](IMG/PW_T.png)

_������������ ����:_
![LOO ��� PW...](IMG/PW_Q.png)

_���������� ����:_
![LOO ��� PW...](IMG/PW_E.png)

�������� ������ ���� ���������� ��� _h_ ���� � ��������� ���������.
���� �������� ������� �� ��������� ���������������� �����. ���� _h_ �������
������� ���������, ���������� "������" ������������ �� ����� �������������
������� ���������. ���� _h_ ������� ������� �������, �� ������ ��������� ���
������������� ������� ������� �����. ������ ����� ��������, ��� � ��������� �����
���� ������� ����� ������� ������, � �������� ��������� ��� ������� _h_ ������ �� ���
������. ��������� ����� (�� ������ ������) �������� _�������������_, ��� ��� ����
���� ����� ������ ���� ����������.

�������� ������������� ���� ����� � ��������� � ���������� (���� �����, ���
__KNN__). �� ����� ���������� ��������� ������ � ������������ ����������,
 ������ ����� ��� ������������ �����������, �������� __KNN__ � __����������
KNN__ �� ��������.

__�����:__
- ����� � ����������
- �������� ���������� ��� ��������� ���������� _h_
- ��� ����� � ���������� ����������� ����� �������
- ������������� ����� ��������
![](http://latex.codecogs.com/svg.latex?%5Clarge%20O%28n%29)
, ��� ��� �� ������� ����������

__������:__
- ���������� ������� ��� ������� �������
- ������ ����� ����������
- � ������ ���������� ����� ������� �������� �������� �����
(������ ����� ��������, ��� ��� ������ ����� ����������� �����)
- �������� ��������� _h_ ���������� ��������� ��������������, ��������
��������� ������������ �����
- ���� �� ���� ����� �� ������ � ������ _h_, �������� �� �������� ��
���������������� (��. �������)! �� ��� ������, ��� ����� ������������
����������