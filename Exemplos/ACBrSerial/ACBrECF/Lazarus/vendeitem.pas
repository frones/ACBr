unit VendeItem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TfrVendeItem }

  TfrVendeItem = class(TForm)
    Bevel1 : TBevel ;
    Bevel2 : TBevel ;
    Button1 : TButton ;
    Button2 : TButton ;
    edCodigo : TEdit ;
    edDescAcres : TEdit ;
    edDescricao : TEdit ;
    edICMS : TEdit ;
    edPrecoUnita : TEdit ;
    edQtd : TEdit ;
    edUN : TEdit ;
    Label1 : TLabel ;
    Label10 : TLabel ;
    Label11 : TLabel ;
    Label12 : TLabel ;
    Label2 : TLabel ;
    Label3 : TLabel ;
    Label4 : TLabel ;
    Label5 : TLabel ;
    Label6 : TLabel ;
    Label7 : TLabel ;
    Label8 : TLabel ;
    Label9 : TLabel ;
    Panel1 : TPanel ;
    rbAcrescimo : TRadioButton ;
    rbDesconto : TRadioButton ;
    rbPercentagem : TRadioButton ;
    rbValor : TRadioButton ;
    procedure edQtdKeyPress(Sender: TObject; var Key: Char);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frVendeItem: TfrVendeItem;

implementation

{$R *.lfm}

uses ECFTeste1;

procedure TfrVendeItem.edQtdKeyPress(Sender: TObject; var Key: Char);
begin
  if Key in [',','.'] then
     Key := DecimalSeparator ;
end;

procedure TfrVendeItem.Button1Click(Sender: TObject);
Var Desc, Tipo : Char ;
begin
//  if Form1.ACBrECF1.AguardandoResposta then
//     raise Exception.Create('Aguarde imprimindo Item anterior...') ;

  Button1.Enabled := False ;
  Tipo := '%' ;
  if rbValor.Checked then
     Tipo := '$' ;
  Desc := 'D' ;
  if rbAcrescimo.Checked then
     Desc := 'A' ;

  try
     Form1.ACBrECF1.VendeItem( edCodigo.Text, edDescricao.Text,
                               edICMS.Text, StrToFloatDef( edQtd.Text, 0 ),
                               StrToFloatDef( edPrecoUnita.Text,0 ),
                               StrToFloatDef( edDescAcres.Text,0 ), edUN.Text,
                               Tipo, Desc );
     Form1.mResp.Lines.Add( 'Vende Item: Cod:'+ edCodigo.Text+
                            ' Desc'+ edDescricao.Text+
                            ' Aliq:'+edICMS.Text +
                            ' Qtd:'+edQtd.Text +
                            ' Preço:'+edPrecoUnita.Text +
                            ' Desc:'+edDescAcres.Text +
                            ' Un:'+edUN.Text +
                            ' Tipo:'+Tipo +
                            ' Desc:'+Desc);
     // Pode desligar ACBrECF1.ArredondaItemMFD caso o comando não exista //
     Form1.chArredondamentoItemMFD.Checked := Form1.ACBrECF1.ArredondaItemMFD;
     Form1.AtualizaMemos ;
  finally
     Button1.Enabled := True ;
  end ;
end;

procedure TfrVendeItem.Button2Click(Sender: TObject);
begin
  close ;
end;

end.

