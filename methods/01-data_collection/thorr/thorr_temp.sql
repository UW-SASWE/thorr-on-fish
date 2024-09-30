SELECT 
    Date, ReachID, RiverID, Name AS RiverName, RKm, EstTempC
FROM
    thorr.ReachData
        LEFT JOIN
    (SELECT 
        RiverID, ReachID, Rivers.Name AS Name, RKm
    FROM
        Reaches
    LEFT JOIN Rivers USING (RiverID)) AS T USING (ReachID)
ORDER BY ReachID , date