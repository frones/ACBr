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

unit CEPTeste1 ;

interface

//** Converted with Mida 600     http://www.midaconverter.com - PROJETO.ACBR



uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.IniFiles,
  Data.DB,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.Objects,
  FMX.Menus,
  FMX.Grid,
  FMX.ExtCtrls,
  FMX.ListBox,
  FMX.TreeView,
  FMX.Memo,
  FMX.TabControl,
  FMX.Layouts,
  FMX.Edit,
  FMX.Platform,
  FMX.Bind.DBEngExt,
  FMX.Bind.Editors,
  FMX.Bind.DBLinks,
  FMX.Bind.Navigator,
  Data.Bind.EngExt,
  Data.Bind.Components,
  Data.Bind.DBScope,
  Data.Bind.DBLinks,
  Datasnap.DBClient,
  Fmx.Bind.Grid,
  System.Rtti,
  System.Bindings.Outputs,
  Data.Bind.Grid,
  Fmx.StdCtrls,
  FMX.Header,
  FMX.Graphics, ACBrIBGE, ACBrBase, ACBrSocket, ACBrCEP,
  FMX.Controls.Presentation, FMX.ScrollBox;

//**   Original VCL Uses section : 


//**   Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
//**   ComCtrls, ExtCtrls, ACBrCEP, ACBrSocket, ACBrIBGE, ACBrBase ;


type

  { TForm1 }

  TForm1 = class(TForm)
    ACBrCEP1 : TACBrCEP ;
    ACBrIBGE1 : TACBrIBGE ;
    bBuscarCEP : TButton ;
    bBuscarCEP1 : TButton ;
    bBuscarLogradouro : TButton ;
    bBuscarLogradouro1 : TButton ;
    cbxWS : TComboBox ;
    edCEP : TEdit ;
    edIBGECod : TEdit ;
    edIBGENome : TEdit ;
    edLogradouro : TEdit ;
    edCidade : TEdit ;
    edTipo_Logradouro : TEdit ;
    edUF : TEdit ;
    edBairro : TEdit ;
    edChaveBuscarCEP : TEdit ;
    edProxyHost : TEdit ;
    edProxyPass : TEdit ;
    edProxyPort : TEdit ;
    edProxyUser : TEdit ;
    GroupBox1 : TGroupBox ;
    GroupBox2 : TGroupBox ;
    GroupBox3 : TGroupBox ;
    GroupBox4 : TGroupBox ;
    GroupBox5 : TGroupBox ;
    GroupBox6 : TGroupBox ;
    Label1 : TLabel ;
    Label10 : TLabel ;
    Label2 : TLabel ;
    Label3 : TLabel ;
    Label4 : TLabel ;
    Label5 : TLabel ;
    Label6 : TLabel ;
    Label7 : TLabel ;
    Label8 : TLabel ;
    Label9 : TLabel ;
    Memo1 : TMemo ;
    PageControl1: TTabControl;
    TabSheet1: TTabItem;
    TabSheet2: TTabItem;
    tsIBGE: TTabItem;
    btEstatisticas: TButton;
    cbIgnorar: TCheckBox;
    procedure ACBrCEP1AntesAbrirHTTP(var AURL : String) ;
    procedure ACBrCEP1BuscaEfetuada(Sender : TObject) ;
    procedure ACBrIBGE1AntesAbrirHTTP(var AURL : String) ;
    procedure ACBrIBGE1BuscaEfetuada(Sender : TObject) ;
    procedure bBuscarCEP1Click(Sender : TObject) ;
    procedure bBuscarCEPClick(Sender : TObject) ;
    procedure bBuscarLogradouro1Click(Sender : TObject) ;
    procedure bBuscarLogradouroClick(Sender : TObject) ;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btEstatisticasClick(Sender: TObject);
  private
    procedure ConfigurarComponente ;
  public

  end ;

var
  Form1 : TForm1 ; 

implementation

uses
  TypInfo;

{$R *.FMX}

{ TForm1 }

procedure TForm1.ConfigurarComponente ;
begin
  ACBrCEP1.WebService := TACBrCEPWebService( cbxWS.ItemIndex ) ;

  ACBrCEP1.ProxyHost   := edProxyHost.Text ;
  ACBrCEP1.ProxyPort   := edProxyPort.Text ;
  ACBrCEP1.ProxyUser   := edProxyUser.Text ;
  ACBrCEP1.ProxyPass   := edProxyPass.Text ;
  ACBrCEP1.ChaveAcesso := edChaveBuscarCEP.Text;

  ACBrIBGE1.ProxyHost := edProxyHost.Text ;
  ACBrIBGE1.ProxyPort := edProxyPort.Text ;
  ACBrIBGE1.ProxyUser := edProxyUser.Text ;
  ACBrIBGE1.ProxyPass := edProxyPass.Text ;
  ACBrIBGE1.IgnorarCaixaEAcentos := cbIgnorar.IsChecked;
end ;

//******************************************************************************

procedure TForm1.FormCreate(Sender: TObject);
var
  Ini: TIniFile;
  I : TACBrCEPWebService ;
begin

  cbxWS.Items.Clear ;
  For I := Low(TACBrCEPWebService) to High(TACBrCEPWebService) do
     cbxWS.Items.Add( GetEnumName(TypeInfo(TACBrCEPWebService), integer(I) ) ) ;

  Ini := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'config.ini');
  try
    cbxWS.ItemIndex       := Ini.ReadInteger('GERAL', 'Modelo', 0);
    edChaveBuscarCEP.Text := Ini.ReadString('GERAL', 'ChaveAcesso', '');

    edProxyHost.Text := Ini.ReadString('PROXY', 'Host', '');
    edProxyPort.Text := Ini.ReadString('PROXY', 'Porta', '');
    edProxyUser.Text := Ini.ReadString('PROXY', 'Usuario', '');
    edProxyPass.Text := Ini.ReadString('PROXY', 'Senha', '');
  finally
    Ini.Free;
  end;

  cbxWS.ItemIndex := 8;
  PageControl1.First;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'config.ini');
  try
    Ini.WriteInteger('GERAL', 'Modelo', cbxWS.ItemIndex);
    Ini.WriteString('GERAL', 'ChaveAcesso', edChaveBuscarCEP.Text);

    Ini.WriteString('PROXY', 'Host', edProxyHost.Text);
    Ini.WriteString('PROXY', 'Porta', edProxyPort.Text);
    Ini.WriteString('PROXY', 'Usuario', edProxyUser.Text);
    Ini.WriteString('PROXY', 'Senha', edProxyPass.Text);
  finally
    Ini.Free;
  end;
end;

//******************************************************************************

procedure TForm1.ACBrCEP1AntesAbrirHTTP(var AURL : String) ;
begin
  Memo1.Lines.Clear;
  Memo1.Lines.Add('Efetuando consulta HTTP em:' ) ;
  Memo1.Lines.Add( AURL );
  Memo1.Lines.Add( '' );
end;

procedure TForm1.ACBrCEP1BuscaEfetuada(Sender : TObject) ;
var
  I : Integer ;
begin
  if ACBrCEP1.Enderecos.Count < 1 then
     Memo1.Lines.Add( 'Nenhum Endereço encontrado' )
  else
   begin
     Memo1.Lines.Add( IntToStr(ACBrCEP1.Enderecos.Count) + ' Endereço(s) encontrado(s)');
     Memo1.Lines.Add('');

     For I := 0 to ACBrCEP1.Enderecos.Count-1 do
     begin
       with ACBrCEP1.Enderecos[I] do
       begin
          Memo1.Lines.Add('CEP: '+CEP );
          Memo1.Lines.Add('Logradouro: '+Tipo_Logradouro+ ' ' +Logradouro );
          Memo1.Lines.Add('Complemento: '+Complemento );
          Memo1.Lines.Add('Bairro: '+Bairro );
          Memo1.Lines.Add('Municipio: '+Municipio + ' - IBGE: '+IBGE_Municipio);
          edCidade.Text := Municipio;
          Memo1.Lines.Add('UF: '+UF + ' - IBGE: '+IBGE_UF);
          Memo1.Lines.Add( StringOfChar('-',20) );
       end ;
     end ;
   end ;

  Memo1.Lines.Add('');
  Memo1.Lines.Add('Resposta HTTP:');
  Memo1.Lines.Add( ACBrCEP1.HTTPResponse );
end;

//******************************************************************************

procedure TForm1.ACBrIBGE1AntesAbrirHTTP(var AURL : String) ;
begin
  Memo1.Lines.Clear;
  Memo1.Lines.Add('Efetuando consulta HTTP em:' ) ;
  Memo1.Lines.Add( AURL );
  Memo1.Lines.Add( '' );
end;

procedure TForm1.ACBrIBGE1BuscaEfetuada(Sender : TObject) ;
var
  I : Integer ;
begin
  if ACBrIBGE1.Cidades.Count < 1 then
     Memo1.Lines.Add( 'Nenhuma Cidade encontrada' )
  else
   begin
     Memo1.Lines.Add( IntToStr(ACBrIBGE1.Cidades.Count) + ' Cidade(s) encontrada(s)');
     Memo1.Lines.Add('');

     For I := 0 to ACBrIBGE1.Cidades.Count-1 do
     begin
       with ACBrIBGE1.Cidades[I] do
       begin
          Memo1.Lines.Add('Cod UF: '+IntToStr(CodUF) );
          Memo1.Lines.Add('UF: '+UF);
          Memo1.Lines.Add('Cod.Município: '+IntToStr(CodMunicipio) );
          Memo1.Lines.Add('Município: '+Municipio );
          Memo1.Lines.Add('Área: '+FormatFloat('0.00', Area) );
          Memo1.Lines.Add( StringOfChar('-',20) );
       end ;
     end ;
   end ;

  Memo1.Lines.Add('');
  Memo1.Lines.Add('Resposta HTTP:');
  Memo1.Lines.Add( ACBrIBGE1.HTTPResponse );
end;

//******************************************************************************

procedure TForm1.bBuscarCEP1Click(Sender : TObject) ;
begin
  Memo1.Lines.Clear;
  ConfigurarComponente ;

  try
     ACBrIBGE1.BuscarPorCodigo( StrToIntDef(edIBGECod.Text,0) );
  except
     On E : Exception do
     begin
        Memo1.Lines.Add(E.Message);
     end ;
  end ;
end;

procedure TForm1.bBuscarCEPClick(Sender : TObject) ;
begin
  ConfigurarComponente ;

  try
     ACBrCEP1.BuscarPorCEP(edCEP.Text);
  except
     On E : Exception do
     begin
        Memo1.Lines.Add(E.Message);
     end ;
  end ;
end;

//******************************************************************************

procedure TForm1.bBuscarLogradouro1Click(Sender : TObject) ;
begin
  Memo1.Lines.Clear;
  ConfigurarComponente ;

  try
     ACBrIBGE1.BuscarPorNome( edIBGENome.Text );
  except
     On E : Exception do
     begin
        Memo1.Lines.Add(E.Message);
     end ;
  end ;
end;

procedure TForm1.bBuscarLogradouroClick(Sender : TObject) ;
begin
  Memo1.Lines.Clear;
  ConfigurarComponente ;

  try
     ACBrCEP1.BuscarPorLogradouro( edCidade.Text, edTipo_Logradouro.Text,
                                   edLogradouro.Text, edUF.Text, edBairro.Text);
  except
     On E : Exception do
     begin
        Memo1.Lines.Add(E.Message);
     end ;
  end ;
end;

procedure TForm1.btEstatisticasClick(Sender: TObject);
begin
   ACBrIBGE1.ObterEstatisticasCidadesUF('SP');
end;

end.

