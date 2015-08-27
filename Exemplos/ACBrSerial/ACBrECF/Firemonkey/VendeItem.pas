unit VendeItem;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation;

type
  TfrVendeItem = class(TForm)
    Label8: TLabel;
    edCodigo: TEdit;
    Label1: TLabel;
    edDescricao: TEdit;
    Label2: TLabel;
    edQtd: TEdit;
    Label3: TLabel;
    edPrecoUnita: TEdit;
    Label4: TLabel;
    edUN: TEdit;
    Panel1: TPanel;
    Label5: TLabel;
    edDesconto: TEdit;
    rbPercentagem: TRadioButton;
    rbValor: TRadioButton;
    Panel2: TPanel;
    Label6: TLabel;
    edICMS: TEdit;
    Label7: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    ckInmetro: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure edQtdKeyPress(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frVendeItem: TfrVendeItem;

implementation
uses
  ECFTeste1, ACBrConsts;

{$R *.fmx}

procedure TfrVendeItem.Button1Click(Sender: TObject);
Var Desc : Char ;
begin
//  if Form1.ACBrECF1.AguardandoResposta then
//     raise Exception.Create('Aguarde imprimindo Item anterior...') ;

  Button1.Enabled := False ;
  Desc := '%' ;
  if rbValor.IsChecked then
     Desc := '$' ;

  try
     if ckInmetro.IsChecked then
        Form1.ACBrECF1.LegendaInmetroProximoItem;

     Form1.ACBrECF1.VendeItem( edCodigo.Text, edDescricao.Text,
                               edICMS.Text, StrToFloatDef( edQtd.Text, 0 ),
                               StrToFloatDef( edPrecoUnita.Text,0 ),
                               StrToFloatDef( edDesconto.Text,0 ), edUN.Text,
                               Desc );

     Form1.mResp.Lines.Add( 'Vende Item: Cod:'+ edCodigo.Text+
                            ' Desc'+ edDescricao.Text+
                            ' Aliq:'+edICMS.Text +
                            ' Qtd:'+edQtd.Text +
                            ' Preço:'+edPrecoUnita.Text +
                            ' Desc:'+edDesconto.Text +
                            ' Un:'+edUN.Text +
                            ' Desc:'+Desc );
     Form1.AtualizaMemos ;
  finally
     Button1.Enabled := True ;
  end ;
end;

procedure TfrVendeItem.Button2Click(Sender: TObject);
begin
  close ;
end;

procedure TfrVendeItem.edQtdKeyPress(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if KeyChar in [',','.'] then
     KeyChar := DecimalSeparator ;
end;

end.
