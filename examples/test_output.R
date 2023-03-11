# Input parameters from sheet Input parameters
probability_stroke_aspirin <- 0.1
probability_stroke_warfarin <- 0.05
probability_bleed_aspirin <- 2.5000000000000001E-2
probability_bleed_warfarin <- 0.05
cost_course_aspirin <- 10
cost_course_warfarin <- 500
cost_stroke <- 800
cost_bleed <- 400
qalys_stroke <- 14
qalys_bleed <- 12
qalys_well <- 20
# Calculate the ICER 
icer <- ((cost_course_warfarin+probability_bleed_warfarin*cost_bleed+probability_stroke_warfarin*cost_stroke)-(cost_course_aspirin+probability_bleed_aspirin*cost_bleed+probability_stroke_aspirin*cost_stroke))/((probability_bleed_warfarin*qalys_bleed+probability_stroke_warfarin*qalys_stroke+(1-probability_bleed_warfarin-probability_stroke_warfarin)*qalys_well)-(probability_bleed_aspirin*qalys_bleed+probability_stroke_aspirin*qalys_stroke+(1-probability_bleed_aspirin-probability_stroke_aspirin)*qalys_well))