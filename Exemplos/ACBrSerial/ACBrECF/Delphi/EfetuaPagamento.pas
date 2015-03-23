{$I ACBr.inc}

unit EfetuaPagamento;

interface

uses ACBrECF, 
  SysUtils,
  {$IFDEF Delphi6_UP} StrUtils, Variants, Types, {$ELSE} ACBrD5,{$ENDIF}
  Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ComCtrls;

type
  TfrPagamento = class(TForm)
    PgPagamento: TPageControl;
    TabSheetPagamento: TTabSheet;
    TabSheetEstorno: TTabSheet;
    SpeedButton1: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lTotalAPAGAR: TLabel;
    lTotalPago: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    mFormas: TMemo;
    edCod: TEdit;
    edValor: TEdit;
    btImprimir: TButton;
    cbVinc: TCheckBox;
    edObs: TEdit;
    Label9: TLabel;
    EdtTipoCanc: TEdit;
    Label10: TLabel;
    EdtTipoNovo: TEdit;
    CbVincCancelado: TCheckBox;
    CBVincNovo: TCheckBox;
    btnEstornar: TButton;
    EdtValor: TEdit;
    Label11: TLabel;
    Label12: TLabel;
    MFormasEst: TMemo;
    Label13: TLabel;
    EdtMsgPromocional: TEdit;
    MemoInformacaoEstorno: TMemo;
    procedure edValorKeyPress(Sender: TObject; var Key: Char);
    procedure btImprimirClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure btnEstornarClick(Sender: TObject);
    procedure MFormasEstEnter(Sender: TObject);
    procedure mFormasEnter(Sender: TObject);
  private
    procedure AtualizaVal ;
    Procedure CarregaFPG ;
  public
    { Public declarations }
    TipoCupom : Char ;
    Estado: String;
  end;

var
  frPagamento: TfrPagamento;

implementation

uses ECFTeste1, ACBrECFClass, ACBrUtil, ACBrConsts;

{$R *.dfm}

procedure TfrPagamento.edValorKeyPress(Sender: TObject; var Key: Char);
begin
  if Key in [',','.'] then
     Key := DecimalSeparator ;
end;

procedure TfrPagamento.btImprimirClick(Sender: TObject);
begin
  if TipoCupom <> 'N' then
   begin
     Form1.ACBrECF1.EfetuaPagamento( edCod.Text, StrToFloat( edValor.Text),
                                     edObs.Text ,cbVinc.Checked );
     Form1.mResp.Lines.Add( 'Efetua Pagamento: '+edCod.Text +
                            ' Valor: '+edValor.Text +
                            ' Obs: '+edObs.Text +
                            ' Vinc: '+IfThen(cbVinc.Checked,'S','N') );
   end
  else
   begin
     Form1.ACBrECF1.EfetuaPagamentoNaoFiscal( edCod.Text, StrToFloat( edValor.Text),
                                     edObs.Text ,cbVinc.Checked );
     Form1.mResp.Lines.Add( 'Efetua Pagamento Não Fiscal: '+edCod.Text +
                            ' Valor: '+edValor.Text +
                            ' Obs: '+edObs.Text +
                            ' Vinc: '+IfThen(cbVinc.Checked,'S','N') );
   end ;

  Form1.AtualizaMemos ;
  AtualizaVal ;
end;

procedure TfrPagamento.FormShow(Sender: TObject);
begin
  if Estado = 'Estorno' then
    PgPagamento.ActivePage:= TabSheetEstorno
  else
    PgPagamento.ActivePage:= TabSheetPagamento;
  Estado:= '';
  
  AtualizaVal ;
  CarregaFPG ;
  TipoCupom := 'F' ;
end;

procedure TfrPagamento.AtualizaVal;
begin
  lTotalAPAGAR.Caption := FloatToStr( Form1.ACBrECF1.Subtotal ) ;
  lTotalPago.Caption   := FloatToStr( Form1.ACBrECF1.TotalPago ) ;
end;

procedure TfrPagamento.SpeedButton1Click(Sender: TObject);
Var Descricao : String ;
    FPG : TACBrECFFormaPagamento ;  { Necessita de uses ACBrECF }
begin
  if InputQuery('Pesquisa Descrição Forma Pagamento',
                'Entre com a Descrição a Localizar ou Cadastrar(Bematech)',
                Descricao) then
  begin
     FPG := Form1.ACBrECF1.AchaFPGDescricao( Descricao ) ;

     if FPG = nil then
        raise Exception.Create('Forma de Pagamento: '+Descricao+
                               ' não encontrada') ;

     edCod.Text := FPG.Indice ;

     { Bematech permite cadastrar formas de Pagamento dinamicamente }
     if (Form1.ACBrECF1.ModeloStr = 'Bematech') and
        (pos( FPG.Descricao, mFormas.Text ) = 0) then
        CarregaFPG ;
  end ;

end;

procedure TfrPagamento.CarregaFPG;
Var A : Integer ;
begin
  mFormas.Clear ;
  mFormas.Clear;
  with Form1 do
  begin
     { Bematech e NaoFiscal permitem cadastrar formas de Pagamento dinamicamente }
     if (Form1.ACBrECF1.Modelo in [ecfBematech,ecfNaoFiscal])then
        ACBrECF1.CarregaFormasPagamento 
     else
        ACBrECF1.AchaFPGIndice('') ;  { força carregar, se ainda nao o fez }
     
     for A := 0 to ACBrECF1.FormasPagamento.Count -1 do
     begin
        mFormas.Lines.Add( ACBrECF1.FormasPagamento[A].Indice+' -> '+
              ACBrECF1.FormasPagamento[A].Descricao+' - '+IfThen(
              ACBrECF1.FormasPagamento[A].PermiteVinculado,'v',''));
        MFormasEst.Lines.Add( ACBrECF1.FormasPagamento[A].Indice+' -> '+
              ACBrECF1.FormasPagamento[A].Descricao+' - '+IfThen(
              ACBrECF1.FormasPagamento[A].PermiteVinculado,'v',''));
     end ;
  end ;

end;

procedure TfrPagamento.btnEstornarClick(Sender: TObject);
begin
   Form1.ACBrECF1.EstornaPagamento(EdtTipoCanc.Text, EdtTipoNovo.Text,
                                   StrToFloat(EdtValor.Text),EdtMsgPromocional.Text);
   Form1.AtualizaMemos ;
   AtualizaVal ;
end;

procedure TfrPagamento.MFormasEstEnter(Sender: TObject);
begin
   btnEstornar.SetFocus;
end;

procedure TfrPagamento.mFormasEnter(Sender: TObject);
begin
   btImprimir.SetFocus;
end;

end.
