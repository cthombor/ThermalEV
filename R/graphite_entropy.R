#' Evaluates an approximation of the entropic shift in the graphite anode
#' of a lithium-ion storage battery as a function of its SOC.  Ref:
#' Chen et al, 2023, DOI: 10.1016/j.applthermaleng.2022.119852;
#' Mei et al, 2019, DOI: 10.1039/c8se00503f
#' Both of these approximations were curve-fits by COMSOL to
#' experimental data.  Chen 2023 acks Mei: "The authors also
#' express graduate to Miss Mei for her guidance and care in
#' completing this paper."  The cubic Mei approximation seems to be
#' based on measurements at only a few SOC, anyway it differs
#' wildly from the 8th-order Chen approximation at low SOC -- whose
#' source is not clearly cited. I'm guessing it's a later result
#' from Mei's group and is more accurate.  The cathodic entropic shifts
#' may dominate, and they vary with the elemental ratios in its NMC
#' composition, so I'm not optimistic about finding COMSOL approximations
#' that will align with the observed behaviour of the packs
#' for which I have extensive LeafSpy csv logfiles.

#' @param SOC in 0-1
#' @param source in c("Chen", "Mei")
#'
#' @returns numeric, units mV/K
#' @export
#'
#' @examples
#' plot(c(0:100)/100, graphite_entropy(c(0:100)/100))
#' plot(c(0:100)/100, graphite_entropy(c(0:100)/100, "Mei"))

graphite_entropy <- function(SOC = 0.5, source = "Chen") {
  if (source == "Chen") {
    sfsmisc::polyn.eval(c(-0.1008, 0.2762, -3.4301, 19.7009, -59.77425,
                          111.39549, -127.9796, 81.462, -21.624),
                        SOC)
  } else if (source == "Mei") {
    sfsmisc::polyn.eval(c(-0.64816, 4.34612, -8.05595, 4.44722),
                        SOC)
  }
}
