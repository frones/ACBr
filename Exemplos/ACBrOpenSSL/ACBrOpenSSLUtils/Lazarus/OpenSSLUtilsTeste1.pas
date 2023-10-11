{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{																			   }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

unit OpenSSLUtilsTeste1 ;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, Menus, ComCtrls, ACBrOpenSSLUtils, OpenSSLExt;

type

  { TForm1 }

  TForm1 = class(TForm)
    ACBrOpenSSLUtils1: TACBrOpenSSLUtils;
    btGerarCSR: TBitBtn;
    btCalcModExp1: TBitBtn;
     btCalcPubKey1: TBitBtn;
     btGerarPubKeyOpenSSH: TBitBtn;
     btLerPubKeyOpenSSH: TBitBtn;
     btCalcMD5 : TBitBtn ;
     btAssinar: TBitBtn;
     btGravarPubKey : TBitBtn ;
     btProcurarArqEntrada : TBitBtn ;
     btLerPubKey : TBitBtn ;
     btCalcPubKey : TBitBtn ;
     btNovoParChaves : TBitBtn ;
     btCalcModExp : TBitBtn ;
     btLerPrivKey : TBitBtn ;
     btGravarPrivKey : TBitBtn ;
     btVersao : TButton ;
     btCryptDecrypt: TButton;
     cbxDgst : TComboBox ;
     cbxOut: TComboBox;
     edArqPrivKey : TEdit ;
     edArqEntrada : TEdit ;
     edSenha: TEdit;
     edArqPubKey : TEdit ;
     gbPrivKey : TGroupBox ;
     gbArqEntrada : TGroupBox ;
     gbPubKey : TGroupBox ;
     GroupBox1 : TGroupBox ;
     Label1 : TLabel ;
     Label2: TLabel;
     Label3: TLabel;
     Label4 : TLabel ;
     Label5 : TLabel ;
     Label6: TLabel;
     mPrivKey : TMemo ;
     mPubKey : TMemo ;
     mResp : TMemo ;
     OpenDialog1 : TOpenDialog ;
     ProgressBar1: TProgressBar;
     SelectDirectoryDialog1 : TSelectDirectoryDialog ;
     procedure ACBrOpenSSLUtils1NeedCredentials(
       const CredentialNeeded: TACBrOpenSSLCredential);
     procedure ACBrOpenSSLUtils1Progress(const PosByte, TotalSize: int64);
     procedure btGerarCSRClick(Sender: TObject);
     procedure btAssinarClick(Sender: TObject);
     procedure btCalcModExp1Click(Sender: TObject);
     procedure btCalcPubKey1Click(Sender: TObject);
     procedure btCalcPubKeyClick(Sender : TObject) ;
     procedure btGerarPubKeyOpenSSHClick(Sender: TObject);
     procedure btGravarPrivKeyClick(Sender : TObject) ;
     procedure btGravarPubKeyClick(Sender : TObject) ;
     procedure btLerPubKeyClick(Sender : TObject) ;
     procedure btLerPubKeyOpenSSHClick(Sender: TObject);
     procedure btNovoParChavesClick(Sender : TObject) ;
     procedure btLerPrivKeyClick(Sender : TObject) ;
     procedure btCalcMD5Click(Sender : TObject) ;
     procedure btCalcModExpClick(Sender : TObject) ;
     procedure btProcurarArqEntradaClick(Sender : TObject) ;
     procedure btVersaoClick(Sender : TObject) ;
     procedure btCryptDecryptClick(Sender: TObject);
     procedure FormCreate(Sender : TObject) ;
     procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end ; 

var
  Form1 : TForm1 ; 

implementation

Uses
  dateutils, TypInfo,
  ACBrUtil, synautil, synacode;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender : TObject) ;
Var
  i: TACBrOpenSSLAlgorithm;
  j: TACBrOpenSSLStrType;
begin
   cbxDgst.Items.Clear ;
   For i := Low(TACBrOpenSSLAlgorithm) to High(TACBrOpenSSLAlgorithm) do
      cbxDgst.Items.Add( copy(GetEnumName(TypeInfo(TACBrOpenSSLAlgorithm), integer(i)), 4, 20) ) ;
   cbxDgst.ItemIndex := 2;

   cbxOut.Items.Clear ;
   For j := Low(TACBrOpenSSLStrType) to High(TACBrOpenSSLStrType) do
      cbxOut.Items.Add( copy(GetEnumName(TypeInfo(TACBrOpenSSLStrType), integer(j)), 4, 20) ) ;
   cbxOut.ItemIndex := 0;

   edArqPrivKey.Text := ApplicationPath + 'priv_key.pem' ;
   edArqPubKey.Text  := ApplicationPath + 'pub_key.pem' ;
   edArqEntrada.Text := Application.ExeName;

   if FileExists( edArqPrivKey.Text ) then
      mPrivKey.Lines.LoadFromFile( edArqPrivKey.Text );

   if FileExists( edArqPubKey.Text ) then
      mPubKey.Lines.LoadFromFile( edArqPubKey.Text );
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  btVersao.Click;
end;

procedure TForm1.btNovoParChavesClick(Sender : TObject) ;
Var
  ChavePublica, ChavePrivada : AnsiString ;
begin
  ChavePrivada := '' ;
  ChavePublica := '' ;

  ACBrOpenSSLUtils.GenerateKeyPair( ChavePrivada, ChavePublica, edSenha.Text);

  mPrivKey.Lines.Text := ChavePrivada;
  mPubKey.Lines.Text  := ChavePublica;

  if MessageDlg('Deseja gravar as chaves geradas ?',mtConfirmation,mbYesNoCancel,0) = mrYes then
  begin
     btGravarPrivKey.Click;
     btGravarPubKey.Click;
  end ;
end;

procedure TForm1.btLerPrivKeyClick(Sender : TObject) ;
begin
  OpenDialog1.FileName := edArqPrivKey.Text;
  if OpenDialog1.Execute then
  begin
     edArqPrivKey.Text := OpenDialog1.FileName;
     mPrivKey.Lines.LoadFromFile( edArqPrivKey.Text );
  end ;
end;

procedure TForm1.btCalcMD5Click(Sender : TObject) ;
var
  Resultado: AnsiString;
  tStart: TDateTime;
begin
   tStart := Now;
   Resultado := ACBrOpenSSLUtils1.CalcHashFromFile( edArqEntrada.Text,
                                       TACBrOpenSSLAlgorithm(cbxDgst.ItemIndex),
                                       TACBrOpenSSLStrType(cbxOut.ItemIndex),
                                       False );

   mResp.Lines.Add('Calculando o HASH - "'+cbxDgst.Text+'" do arquivo: '+edArqEntrada.Text );
   mResp.Lines.Add(UpperCase(cbxDgst.Text)+' = '+ Resultado );
   mResp.Lines.Add('Tempo Gasto: '+IntToStr(SecondsBetween(tStart, Now))+ ' segundos');
   mResp.Lines.Add('------------------------------');
end;

procedure TForm1.btCalcModExpClick(Sender : TObject) ;
Var
  Modulo, Expoente : AnsiString ;
begin
   Modulo   := '';
   Expoente := '';
   ACBrOpenSSLUtils1.ExtractModulusAndExponentFromPublicKey(Modulo, Expoente);

   mResp.Lines.Add('Modulo');
   mResp.Lines.Add( Modulo );
   mResp.Lines.Add('');

   mResp.Lines.Add('Expoente');
   mResp.Lines.Add( Expoente );
   mResp.Lines.Add('------------------------------');
end;

procedure TForm1.btProcurarArqEntradaClick(Sender : TObject) ;
begin
   OpenDialog1.FileName := edArqEntrada.Text;
   if OpenDialog1.Execute then
      edArqEntrada.Text := OpenDialog1.FileName;
end;

procedure TForm1.btVersaoClick(Sender : TObject) ;
begin
   mResp.Lines.Add('Versão OpenSSL');
   mResp.Lines.Add( ACBrOpenSSLUtils.OpenSSLFullVersion );
   mResp.Lines.Add('------------------------------');
end;

procedure TForm1.btCryptDecryptClick(Sender: TObject);
var
  s: AnsiString;
begin
  s := '=== PROJETO ACBR, TESTE 123 ===';
  mResp.Lines.Add('String de teste');
  mResp.Lines.Add(s);

  mResp.Lines.Add('');
  mResp.Lines.Add('Criptografando a String com Chave Publica (SHA256, PKCS1_OAEP_PADDING, resultado em Hexa)');
  s := ACBrOpenSSLUtils1.PublicEncryptFromString(s, algSHA256);
  mResp.Lines.Add(ConvertToStrType(s, sttHexa));

  mResp.Lines.Add('');
  mResp.Lines.Add('Descriptografando a String acima, com a Chave Privada');
  s := ACBrOpenSSLUtils1.PrivateDecryptFromString(s, algSHA256);
  mResp.Lines.Add(s);
end;

procedure TForm1.ACBrOpenSSLUtils1NeedCredentials(
  const CredentialNeeded: TACBrOpenSSLCredential);
begin
  case CredentialNeeded of
    crePrivKey:
    begin
      mResp.Lines.Add('  Lendo Chave Privada');
      ACBrOpenSSLUtils1.LoadPrivateKeyFromString(mPrivKey.Lines.Text, edSenha.Text);
    end;

    crePubKey:
    begin
      mResp.Lines.Add('  Lendo Chave Publica');
      ACBrOpenSSLUtils1.LoadPublicKeyFromString(mPubKey.Lines.Text);
    end;
  end;
end;

procedure TForm1.ACBrOpenSSLUtils1Progress(const PosByte, TotalSize: int64);
begin
  ProgressBar1.Position := trunc(PosByte / TotalSize * 100);
  ProgressBar1.Visible  := (ProgressBar1.Position < 100);
  Application.ProcessMessages;
end;

procedure TForm1.btGerarCSRClick(Sender: TObject);
var
  CN, O, OU, L, ST, C, EMAIL, CSR: String ;
begin
  CN := '*.projetoacbr.com.br';
  if not InputQuery('Gerar CSR','Informe o Common Name (CN)', CN) then
     exit ;

  O :=  'Projeto ACBr';
  if not InputQuery('Gerar CSR','Informe o Organization Name (O)', O) then
     exit ;

  OU :=  'ACBrLab';
  if not InputQuery('Gerar CSR','Informe o Organizational Unit (OU)', OU) then
     exit ;

  L :=  'Tatuí';
  if not InputQuery('Gerar CSR','Informe o Locality (L)', L) then
     exit ;

  ST :=  'São Paulo';
  if not InputQuery('Gerar CSR','Informe o State (ST)', ST) then
     exit ;

  C :=  'BR';
  if not InputQuery('Gerar CSR','Informe o Country (C)', C) then
     exit ;

  EMAIL :=  'sac@projetoacbr.com.br';
  if not InputQuery('Gerar CSR','Informe o e-mail (EMAIL)', EMAIL) then
     exit ;

  mResp.Lines.Add('Gerando o Certificate Sign Request (CSR)');

  CSR := ChangeLineBreak( ACBrOpenSSLUtils1.CreateCertificateSignRequest(CN,O,OU,L,ST,C,EMAIL, algSHA512), sLineBreak);
  mResp.Lines.Add('Certificate Sign Request (CSR):');
  mResp.Lines.Add( CSR );
end;

procedure TForm1.btAssinarClick(Sender: TObject);
var
  Resultado: AnsiString;
  tStart: TDateTime;
begin
   tStart := Now;
   Resultado := ACBrOpenSSLUtils1.CalcHashFromFile( edArqEntrada.Text,
                                       TACBrOpenSSLAlgorithm(cbxDgst.ItemIndex),
                                       TACBrOpenSSLStrType(cbxOut.ItemIndex),
                                       True );

   mResp.Lines.Add('Calculando a Assinatura - "'+cbxDgst.Text+'" do arquivo: '+edArqEntrada.Text );
   mResp.Lines.Add(UpperCase(cbxDgst.Text)+' = '+ Resultado );
   mResp.Lines.Add('Tempo Gasto: '+IntToStr(SecondsBetween(tStart, Now))+ ' segundos');
   mResp.Lines.Add('------------------------------');
end;

procedure TForm1.btCalcModExp1Click(Sender: TObject);
var
  Modu, Expo, Chave: String ;
begin
  Modu := '';
  Expo := '10001';
  if not InputQuery('Módulo','Cole aqui o Módulo', Modu) then
     exit ;

  if not InputQuery('Expoente','Cole aqui o Expoente', Expo) then
     exit ;

  mResp.Lines.Add('Calculando Chave Pública');
  mResp.Lines.Add('Modulo: ' + Modu);
  mResp.Lines.Add('Expoente: ' + Expo);

  ACBrOpenSSLUtils1.LoadPublicKeyFromModulusAndExponent(Modu, Expo);

  Chave := ChangeLineBreak( ACBrOpenSSLUtils1.PublicKeyAsString, sLineBreak);
  mResp.Lines.Add('Chave Pública Calculada:');
  mResp.Lines.Add( Chave );
  mResp.Lines.Add('------------------------------');

  mPubKey.Lines.Text := Chave;
end;

procedure TForm1.btCalcPubKey1Click(Sender: TObject);
var
  Senha, ChavePrivada, ChavePublica: String;
begin
   ChavePrivada := '';
   ChavePublica := '';

   OpenDialog1.Filter := 'pfx|*.pfx';
   if OpenDialog1.Execute then
   begin
      Senha := '' ;
      if not InputQuery('Senha','Entre com a Senha do Certificado', Senha ) then
         exit ;

     ACBrOpenSSLUtils1.LoadPFXFromFile(OpenDialog1.FileName, Senha);
     mPrivKey.Lines.Text := ACBrOpenSSLUtils1.PrivateKeyAsString;
     mPubKey.Lines.Text  := ACBrOpenSSLUtils1.PublicKeyAsString;
   end ;
end;

procedure TForm1.btCalcPubKeyClick(Sender : TObject) ;
var
  Chave: String;
begin
   Chave := ChangeLineBreak( ACBrOpenSSLUtils1.GeneratePublicKeyFromPrivateKey, sLineBreak);
   mResp.Lines.Add('Calculando Chave Pública através da Chave Privada');
   mResp.Lines.Add('Chave Pública Calculada:');
   mResp.Lines.Add( Chave );
   mResp.Lines.Add('------------------------------');

   mPubKey.Lines.Text := Chave;
end;

procedure TForm1.btGerarPubKeyOpenSSHClick(Sender: TObject);
begin
  mResp.Lines.Add( '--------------- Chave Pública no formato do OpenSSH --------------- ');
  mResp.Lines.Add( ACBrOpenSSLUtils1.PublicKeyAsOpenSSH );
  mResp.Lines.Add( '' );
end;

procedure TForm1.btGravarPrivKeyClick(Sender : TObject) ;
begin
   if (edArqPrivKey.Text <> '') and (mPrivKey.Lines.Count > 0) then
   begin
      if FileExists( edArqPrivKey.Text )  then
         if MessageDlg( 'Arquivo já existe, sobrescrever ?',
                        mtConfirmation, mbYesNoCancel, 0) <> mrYes then
            exit ;

      mPrivKey.Lines.SaveToFile( edArqPrivKey.Text );
      MessageDlg( 'Chave Privada salva como:'+sLineBreak+edArqPrivKey.Text,
                  mtInformation, [mbOK], 0 );
   end ;
end;

procedure TForm1.btGravarPubKeyClick(Sender : TObject) ;
begin
   if (edArqPubKey.Text <> '') and (mPubKey.Lines.Count > 0) then
   begin
      if FileExists( edArqPubKey.Text )  then
         if MessageDlg( 'Arquivo já existe, sobrescrever ?',
                        mtConfirmation, mbYesNoCancel, 0) <> mrYes then
            exit ;

      mPubKey.Lines.SaveToFile( edArqPubKey.Text );
      MessageDlg('Chave Pública salva como:'+sLineBreak+edArqPubKey.Text,
                  mtInformation, [mbOK], 0 );
   end ;
end;

procedure TForm1.btLerPubKeyClick(Sender : TObject) ;
begin
   OpenDialog1.FileName := edArqPubKey.Text;
   if OpenDialog1.Execute then
   begin
      edArqPubKey.Text := OpenDialog1.FileName;
      mPubKey.Lines.LoadFromFile( edArqPubKey.Text );
   end ;
end;

procedure TForm1.btLerPubKeyOpenSSHClick(Sender: TObject);
var
   PubKeyOpenSSH: String ;
begin
  PubKeyOpenSSH := '' ;
  if not InputQuery('OpenSSH','Cole aqui o conteudo da Linha da Chave Publica', PubKeyOpenSSH ) then
     exit ;

  mResp.Lines.Add( '--------------- Chave Pública de OpenSSH, convertida para OpenSSL --------------- ');
  mResp.Lines.AddText( ConvertOpenSSHToPEM(PubKeyOpenSSH) );
  mResp.Lines.Add( '' );
end;

end.

