unit uDMFast;

interface

uses
  SysUtils, Classes, ACBrBoleto, ACBrBoletoFCFR, ACBrBase;

type
  TdmFast = class(TDataModule)
    ACBrBoleto: TACBrBoleto;
    ACBrBoletoReport: TACBrBoletoFCFR;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmFast: TdmFast;

implementation

{$R *.dfm}

end.
