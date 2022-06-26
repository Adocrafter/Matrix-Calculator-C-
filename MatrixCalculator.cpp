#include <iostream>
#include <vector>
#include <algorithm>
#include <string>
#include <stack>
#include <cstdlib>
#include <cmath>
using namespace std;

class Matrica
{
private:
    int red;
    int kolona;
    vector<vector<double>> mat={{}};
public:
    Matrica (vector<vector<double>> &m)
    {
        red = m.size();
        if (red<=0) kolona = 0;
        else kolona = m[0].size();
        for (int i = 0; i < red; i++)
        {
            vector<double> m1;
            for (int j = 0; j < kolona; j++)
            {
                m1.push_back(m[i][j]);
            }
            mat.push_back(m1);
        }
    }
    Matrica ()
    {
        red = 0;
        kolona = 0;
    }
    void getRed () {cout<<red;}
    double Determinanta ();
    friend vector<vector<double>> Evaluator(string &s, Matrica &m);
    void setMatricu (vector<vector<double>> &m)
    {
        for (int i =0; i<m.size(); i++)
        {
            vector<double> red4;
            for (int j=0; j< m[0].size(); j++)
            {
                red4.push_back(m[i][j]);
            }
            mat.push_back(red4);
        }
    }

   friend ostream& operator<<(ostream& os, Matrica &m);
   friend istream& operator>> (istream& in, Matrica& m);



};
void ispisiVektor (vector<vector<double>> &v);
vector<vector<double>> MnoziMatSaBrojem (const vector<vector<double>> &mat, double a);
vector<vector<double>> StepenujMat (const vector<vector<double>> &mat, int a);
vector<vector<double>> MnozenjeMat(vector<vector<double>> A, vector<vector<double>> B);
vector<vector<double>> Inverzna (vector<vector<double>>&mat);
vector<vector<double>> Saberi (vector<vector<double>>&mat,vector<vector<double>>&mat2);
vector<vector<double>> Oduzmi (vector<vector<double>>&mat,vector<vector<double>>&mat2);
double PretvoriBroj (string &s);
vector<vector<double>> Transponovana (vector<vector<double>>&mat);
string MatUString (vector<vector<double>> &mat)
{
    string s= "[";
    for (int i =0; i< mat.size();i++)
    {
        for (int j =0; j < mat[0].size();j++)
        {
            string b = to_string (mat[i][j]);
            s = s +  b + " ";
        }
       if (i!=mat.size()-1) s+=";";
    }
    s+="]";
    return s;
}

ostream& operator<<(ostream& os, Matrica &m)
{
    for (int i = 0; i<m.mat.size(); i++)
    {
        for (int j=0; j <m.mat[0].size(); j++)
        {
            os << m.mat[i][j]<<" ";
        }
        os << endl;
    }
    return os;
}
vector<vector<double>> Evaluator(string &s, Matrica &m)
{
    s+= " ";
    string newS = "";
    stack<char> znakovi;
    stack<double> brojevi;
    vector<vector<vector<double>>> matrice;
    vector<double> redovi;
    stack<char> zagrade;
    for (int i = 0; i <s.size();i++)
    {
        if (s[i]=='[' or s[i]=='(') zagrade.push(s[i]);
        if (s[i]==')' or s[i]==']')
        {
            if (s[i]==']')
            {
                if(zagrade.top()=='[') zagrade.pop();
                else throw "pogresno poklopljene zagrade";
            }
            if (s[i]==')')
            {
                if(zagrade.top()=='(') zagrade.pop();
                else throw "Pogresan poklopljene zagrade";
            }
        }
        if (s[i]=='+' or s[i]=='*' or s[i]=='^' or s[i]=='+')
        {
            if (i==0 or i==s.size()-1) throw "znak na pocetku";
            if (s[i-1]=='+' or s[i-1]=='*' or s[i-1]=='^' or s[i-1]=='+') throw "dupli znak";
            if (s[i+1]=='T' and s[i]!='^') throw "pogresno transponovana matrica";
            if (s[i-1]=='T' and s[i]=='^') throw "Pogresan transponovana matrica";
        }

    }
    if (!zagrade.empty())
    {
        throw "viska zagrade";
    }
    string s3="";
    string s4 = "";
    for (int k = 0; k < s.size();k++)
    {
        if (s[k]=='(')
        {
            k++;
            while (s[k]!=')')
            {
                newS+=s[k];
                k++;
            }
            vector<vector<double>> tempmat2;
            tempmat2 = Evaluator(newS,m);
            s3 = MatUString (tempmat2);
            s4  += s3;
        }
        else s4 += s[k];
    }

    int i = 0;
    s = s4;
    while (i<s.size())
    {
        if(s[i]=='[')
        {
            vector<vector<double>> m1;
            while (s[i]!=']' and i<s.size())
            {
                if(s[i]!=';')
                {
                    string pomocni = "";
                    if (isdigit(s[i]))
                    {
                        while (s[i]!=' ' and s[i]!=']' and s[i]!='\n' and i<s.size() and s[i]!=';')
                        {
                            if (s[i]=='.' or isdigit(s[i]))
                            {
                                pomocni+=s[i];
                            }

                            i++;
                        }
                        double broj = PretvoriBroj(pomocni);
                        pomocni = "";
                        redovi.push_back(broj);
                        if (s[i]==']' or s[i-1]==';')
                        {
                            i--;
                            break;
                        }
                    }
                }
                if (s[i]==';')
                {
                    m1.push_back(redovi);
                    while (!redovi.empty()) redovi.pop_back();
                }
                i++;
            }
            m1.push_back(redovi);
            while (!redovi.empty()) redovi.pop_back();
            matrice.push_back(m1);

        }
        else if (s[i]=='*' or s[i]=='+' or s[i]=='-') znakovi.push(s[i]);
        else if (s[i]=='^')
        {
            if (s[i+1]=='T')
            {
                vector<vector<double>> zamjenskamat = Transponovana(matrice[matrice.size()-1]);
                matrice.pop_back();
                matrice.push_back(zamjenskamat);
                i++;
            }
            else if (s[i+1]!='-')
            {
                int brojint = s[i+1]-48;
                matrice[matrice.size()-1] = StepenujMat(matrice[matrice.size()-1],brojint);
                i++;

            }
            else
            {
                matrice[matrice.size()-1] = Inverzna(matrice[matrice.size()-1]);
                int brojint = s[i+2]-48;
                 matrice[matrice.size()-1] = StepenujMat(matrice[matrice.size()-1],brojint);
                i+=2;
            }
        }
        else if (isdigit(s[i]))
        {
            if (!znakovi.empty())
            {
                int brojint = s[i]-48;
                matrice[matrice.size()-1] = MnoziMatSaBrojem(matrice[matrice.size()-1],brojint);
                znakovi.pop();
            }
        }
        i++;
    }
    while (!znakovi.empty())
    {
        if (znakovi.top()=='+')
        {
            vector<vector<double>> mat3 = Saberi(matrice[matrice.size()-1],matrice[matrice.size()-2]);
            matrice.pop_back();
            matrice.pop_back();
            znakovi.pop();
            matrice.push_back(mat3);
        }
        else if (znakovi.top()=='*')
        {
            vector<vector<double>> mat3 = MnozenjeMat(matrice[matrice.size()-1],matrice[matrice.size()-2]);
            matrice.pop_back();
            matrice.pop_back();
            znakovi.pop();
            matrice.push_back(mat3);
        }
        else if (znakovi.top()=='-')
        {
            vector<vector<double>> mat3 = Oduzmi(matrice[matrice.size()-1],matrice[matrice.size()-2]);
            matrice.pop_back();
            matrice.pop_back();
            znakovi.pop();
            matrice.push_back(mat3);
        }
    }
    m.mat = matrice[0];
    return matrice[0];

}
istream& operator>> (istream& in, Matrica& m)
{
    string s = "";
    getline(cin,s);
    try
    {
       vector<vector<double>> mat2 =(Evaluator (s,m));
       m.setMatricu(mat2);

    }
    catch (const char* poruka)
    {
        cout<<poruka<<endl;
    }
    return in;
}

double determinantaRek (vector<vector<double>>&m)
{
    double det = 0;
        if (m.size() == 1)
        {
            return m[0][0];
        }
        else if (m.size() == 2)
        {

            det = (m[0][0] * m[1][1] - m[0][1] * m[1][0]);
            return det;
        }
        else
        {

            for (int p = 0; p < m[0].size(); p++)
            {
                vector<vector<double>> pomocnaMat;
                for (int i = 1; i < m.size(); i++)
                {
                    vector<double> pomocniRed;
                    for (int j = 0; j < m[i].size(); j++)
                    {
                        if (j != p)
                        {
                           pomocniRed.push_back(m[i][j]);
                        }
                    }
                    if (pomocniRed.size() > 0)
                        pomocnaMat.push_back(pomocniRed);
                }
                det = det + m[0][p] * pow(-1, p) * determinantaRek(pomocnaMat);
            }
            return det;
        }
}

vector<vector<double>> Kofaktor (vector<vector<double>>&m)
{
    double kofaktor = 0;
        if (m.size() == 1)
        {
            return m;
        }
        else
        {
            vector<vector<double>>kofaktorMat;
            for (int q = 0; q < m.size(); q++)
            {
                vector<double> kofaktorRed;
                for (int p = 0; p < m[0].size(); p++)
                {
                    vector<vector<double>> pomocnaMat;
                    for (int i = 0; i < m.size(); i++)
                    {
                        vector<double> pomocniRed;
                        for (int j = 0; j < m[i].size(); j++)
                        {
                            if (j != p and i!=q)
                            {
                               pomocniRed.push_back(m[i][j]);
                            }
                        }
                        if (pomocniRed.size() > 0)
                            pomocnaMat.push_back(pomocniRed);
                    }
                    kofaktor = pow(-1, p+q) * determinantaRek(pomocnaMat);
                    kofaktorRed.push_back(kofaktor);
                }
                kofaktorMat.push_back(kofaktorRed);
            }
            return kofaktorMat;
        }
}

double Matrica::Determinanta ()
{
    if (red!=kolona) throw "Matrica nije kvadratna";
    return determinantaRek (mat);

}


void ispisiVektor (vector<vector<double>> &v)
{
    for (int i=0;i<v.size();i++)
    {
        for (int j=0;j<v[0].size();j++)
        {
            cout<<v[i][j]<<" ";
        }
        cout<<endl;
    }
}

vector<vector<double>> Transponovana (vector<vector<double>>&mat)
{
    vector<vector<double>> mat2;
    for (int i=0;i<mat[0].size();i++)
    {
       vector <double> red2;
       for (int j=0;j<mat.size();j++)
       {
           red2.push_back(mat[j][i]);
       }
       mat2.push_back(red2);
    }
    return mat2;
}
vector<vector<double>> Inverzna (vector<vector<double>>&mat)
{
    if(mat.size()!=mat[0].size()) throw "Matrica nije kvadratna";
    double pomocna = 1/determinantaRek(mat);
    vector<vector<double>> mat3;
    mat3 = (Kofaktor(mat));
    mat3= Transponovana(mat3);
    return (MnoziMatSaBrojem(mat3,pomocna));

}

void add(vector<vector<double>> &A, vector<vector<double>> &B, vector<vector<double>> &C, int size)
{
    int i, j;
    for (i = 0; i < size; i++)
    {
        for (j = 0; j < size; j++)
        {
            C[i][j] = A[i][j] + B[i][j];
        }
    }
}

void sub(vector<vector<double>> &A, vector<vector<double>> &B, vector<vector<double>> &C, int size)
{
    int i, j;
    for (i = 0; i < size; i++)
    {
        for (j = 0; j < size; j++)
        {
            C[i][j] = A[i][j] - B[i][j];
        }
    }
}
vector<vector<double>> Saberi (vector<vector<double>>&A,vector<vector<double>>&B)
{
    vector<vector<double>> C(A.size(),A[0]);
    for (int i = 0; i < A.size(); i++)
    {
        for (int j = 0; j < A[0].size(); j++)
        {
            C[i][j] = A[i][j] + B[i][j];
        }
    }
    return C;
}
vector<vector<double>> Oduzmi (vector<vector<double>>&A,vector<vector<double>>&B)
{
    vector<vector<double>> C(A.size(),A[0]);
    for (int i = 0; i < A.size(); i++)
    {
        for (int j = 0; j < A[0].size(); j++)
        {
            C[i][j] = A[i][j] - B[i][j];
        }
    }
    return C;
}


double PretvoriBroj (string &s)
{
    int brojac = -1;
    int brojacdec = -1;
    bool dec = false;
    double broj;
    for (int i=0; i < s.size();i++)
    {
        if (isdigit(s[i]))
        {
            if (!dec) brojac++;
        }
        else dec = true;
    }
    dec = false;
    for (int i=0; i < s.size();i++)
    {
        if (isdigit(s[i]))
        {
            int broj2 = s[i] - 48;
            if (!dec)
            {
                broj+=broj2*pow(10,brojac);
                brojac--;
            }
            else
            {
                broj+=broj2*pow(10,brojacdec);
                brojacdec--;
            }
        }
        else dec = true;
    }
    return broj;
}
vector<vector<double>> MnoziMatSaBrojem (const vector<vector<double>> &mat,  double a)
{
    vector<vector<double>> tempmat;
    for (int i=0;i<mat.size();i++)
    {
        vector<double> tempred;
        for (int j=0;j<mat[0].size();j++)
        {
            tempred.push_back(a*mat[i][j]);
        }
        tempmat.push_back(tempred);
    }
    return tempmat;
}
vector<vector<double>> MnozenjeMat(vector<vector<double>> A, vector<vector<double>> B)
{
    vector<double> red2(A[0].size());
    vector<vector<double>> C(B.size(),red2);
    cout<<endl;
    for (int i = 0; i <B.size(); i++)
    {
       for (int j = 0; j <A[0].size(); j++)
       {
          C[i][j] = 0;
          for (int k = 0; k < B.size(); k++)
          {
             C[i][j] += B[i][k] * A[k][j];
          }
       }
    }
    return C;
}
vector<vector<double>> StepenujMat (const vector<vector<double>> &mat, int a)
{

    vector<vector<double>> mat2;
    for (int i =0; i <mat.size();i++)
    {
        vector<double> red2;
        for (int j =0; j <mat[0].size();j++)
        {
            red2.push_back(mat[i][j]);
        }
        mat2.push_back(red2);

    }
    for (int i = 1; i < a;i++)
    {
        mat2 = MnozenjeMat(mat2,mat);

    }
    return mat2;
}
int main ()
{
    //vector<vector<double>> v {{4,2,3},{4,5,6},{7,8,9}};
   // vector<vector<double>> v1 =  Kofaktor(v);
    //Matrica m(v);
    Matrica m2;

    cin>>m2;
    cout<<m2;



return 0;

}
